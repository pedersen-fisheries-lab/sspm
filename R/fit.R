#' Fit the GAM part of a sspm model
#'
#' Once formulas have been mapped onto a sspm discrete object, the GAMs can be
#' fitted with this function. Arguments can be passed onto `bam`.
#'
#' @inheritParams spm_smooth
#' @inheritParams mgcv::bam
#' @inheritDotParams mgcv::bam
#'
#' @rdname fit
#' @export
setGeneric(name = "fit_smooths",
           def = function(sspm_object,
                          boundaries,
                          keep_fit = TRUE,
                          predict = TRUE,
                          family = mgcv::tw,
                          drop.unused.levels = F,
                          method = "fREML",
                          ...){
             standardGeneric("fit_smooths")
           }
)

#' @rdname fit
#' @export
setGeneric(name = "fit_spm",
           def = function(sspm_object,
                          keep_fit = TRUE,
                          family = mgcv::scat,
                          select = TRUE,
                          method="REML",
                          ...){
             standardGeneric("fit_spm")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname fit
setMethod(f = "fit_smooths",
          signature(sspm_object = "sspm_data",
                    boundaries = "sspm_discrete_boundary"),
          function(sspm_object, boundaries,
                   keep_fit, predict, family, drop.unused.levels, method,  ...){

            # Initialize/collect smoothed_data
            full_smoothed_data <- sspm_object@smoothed_data
            if (is.null(full_smoothed_data)){
              full_smoothed_data <- data.frame()
            }

            # Get data
            the_data <- spm_data(sspm_object)

            # If predict, make the predict matrix
            if(predict){

              min_year <-
                min(as.numeric(as.character(the_data[[spm_time_column(sspm_object)]])),
                    na.rm = TRUE)
              max_year <-
                max(as.numeric(as.character(the_data[[spm_time_column(sspm_object)]])),
                    na.rm = TRUE)

              time_col <- spm_time_column(sspm_object)
              predict_mat <- boundaries@patches %>%
                sf::st_set_geometry(NULL) %>%
                tidyr::expand_grid(!!time_col := min_year:max_year)

            }

            formulas <- spm_formulas(sspm_object)
            formula_length <- length(formulas)

            tmp_fit <-
              vector(mode = "list", length = formula_length)
            tmp_smoothed <-
              vector(mode = "list", length = formula_length)

            for (form_id in seq_len(length.out = formula_length)){

              # Index formula
              form <- formulas[[form_id]]

              if(is_fitted(form)){
                next
              }

              form_name <- paste0(spm_name(sspm_object), "_f", form_id)
              form_vars <- formula_vars(form)

              # Inject name
              names(tmp_fit)[form_id] <- form_name
              names(tmp_smoothed)[form_id] <- form_name

              # Print info
              cli::cli_alert_info(
                paste0(" Fitting formula: ",
                       cli::col_yellow(format_formula(raw_formula(form))),
                       " for dataset ", cli::col_cyan(paste0("'", spm_name(sspm_object),"'"))))

              # Modify formula env, best solution for now
              form_env <- attr(translated_formula(form), ".Environment")
              for(var in names(form_vars)){
                assign(x = var, value = form_vars[[var]], envir = form_env)
              }

              # Fit the formula, important to attach the vars
              tmp_fit[[form_name]] <- tryCatch({
                mgcv::bam(formula = translated_formula(form),
                          data = the_data,
                          family = family,
                          drop.unused.levels = drop.unused.levels,
                          method = method,
                          ...)
              }, error = function(e){

                if (e$message == "Can't find by variable"){
                  cli::cli_alert_danger(" mgcv failed to fit 'by' smooths")
                  cli::cli_alert_info(" Please ensure that all 'by = ...' variables are encoded as factors")
                  stop("mgcv failed to fit 'by' smooths", call. = FALSE)
                } else {
                  stop(e)
                }

              })
            }

            # Note as fitted
            is_fitted(formulas[[form_id]]) <- TRUE
            spm_formulas(sspm_object) <- formulas

            # Store results at dataset level
            if(keep_fit){
              spm_smoothed_fit(sspm_object) <- tmp_fit
            }

            # Predict and store smoothed data to sspm level
            if(predict){

              preds <- predict(tmp_fit[[form_name]],
                               predict_mat, type = "response")

              response <- form@response

              column_name <- paste0(response, "_smooth")

              preds_df <- predict_mat %>%
                dplyr::mutate(!!column_name := as.vector(preds)) %>%
                dplyr::arrange(!!time_col) %>%
                dplyr::group_by(.data$patch_id)

              if (nrow(full_smoothed_data) == 0){

                full_smoothed_data <- preds_df %>%
                  dplyr::left_join(boundaries@patches, by = c("patch_id"),
                                   suffix = c("", "_duplicate")) %>%
                  dplyr::select(-c(dplyr::ends_with("_duplicate")))

              } else {

                full_smoothed_data <- full_smoothed_data %>%
                  dplyr::left_join(preds_df, by = c("patch_id", time_col),
                                   suffix = c("", "_duplicate")) %>%
                  dplyr::select(-c(dplyr::ends_with("_duplicate")))

              }

            }

            nrow_smoothed_data <- nrow(full_smoothed_data)
            full_smoothed_data_clean <- full_smoothed_data %>%
              dplyr::relocate(names(boundaries@boundaries),
                              .after = dplyr::last_col()) %>%
              dplyr::relocate(dplyr::contains("smooth")) %>%
              dplyr::ungroup() %>%
              dplyr::mutate("row_ID" = 1:nrow_smoothed_data) %>%
              dplyr::relocate(.data$row_ID) %>%
              sf::st_as_sf()

            spm_smoothed_data(sspm_object) <- full_smoothed_data_clean
            spm_boundaries(sspm_object) <- boundaries

            return(sspm_object)

          }
)

#' @export
#' @rdname fit
setMethod(f = "fit_spm",
          signature(sspm_object = "sspm_discrete"),
          function(sspm_object, keep_fit, family, select, method, ...){

            # Here we fit the full spm
            # Get dataset
            smoothed_data <- spm_smoothed_data(sspm_object)

            # Check if all datasets have a mapped formula
            formulas <- spm_formulas(smoothed_data)
            formula_length <- length(formulas)
            has_no_formulas <- formula_length < 1

            if(has_no_formulas){
              cli::cli_alert_danger("No mapped formulas for smoothed data")
              stop("No mapped formulas.", call. = FALSE)
            }

            # Initialize/collect smoothed_data
            the_fit <- NULL
            time_col_name <- spm_time_column(smoothed_data)

            # Get data
            the_data <- spm_data(smoothed_data) %>%
              dplyr::filter(.data$train_test == TRUE)

            tmp_fit <-
              vector(mode = "list", length = sum(formula_length))
            tmp_smoothed <-
              vector(mode = "list", length = sum(formula_length))

            for (form_id in seq_len(length.out = formula_length)){

              # Index formula
              form <- formulas[[form_id]]
              form_name <- paste0(spm_name(smoothed_data), "_f", form_id)
              form_vars <- formula_vars(form)

              # Inject name
              names(tmp_fit)[form_id] <- form_name
              names(tmp_smoothed)[form_id] <- form_name

              # Print info
              cli::cli_alert_info(
                paste0(" Fitting SPM formula: ",
                       cli::col_yellow(format_formula(raw_formula(form)))))

              # Modify formula env, best solution for now
              form_env <- attr(translated_formula(form), ".Environment")
              for(var in names(form_vars)){
                assign(x = var, value = form_vars[[var]], envir = form_env)
              }

              # Fit the formula, important to attach the vars
              tmp_fit[[form_name]] <-
                mgcv::bam(formula = translated_formula(form),
                          data = c(as.list(the_data),
                                   form_vars),
                          family = family,
                          select = select,
                          method = method,
                          ...)
            }

            # Store results at smoothed_data level
            if(keep_fit){
              spm_smoothed_fit(smoothed_data) <- tmp_fit
            }

            # TODO not used anymore
            is_smoothed(smoothed_data) <- TRUE

            spm_smoothed_data(sspm_object) <- smoothed_data

            # For now return fit
            return(sspm_object)

          }
)


