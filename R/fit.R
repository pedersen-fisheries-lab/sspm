#' Fit the GAM part of a sspm model
#'
#' Once formulas have been mapped onto a sspm discrete object, the GAMs can be
#' fitted with this function. Arguments can be passed onto `bam`.
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'   [sspm_discrete][sspm_discrete-class].
#' @param predict **\[logical\]** Whether or not to generate the smoothed
#'   predictions (necessary to fit the final SPM model, default to TRUE).
#' @param keep_fit **\[logical\]** Whether or not to keep the fitted values and
#'   model (default to TRUE, set to FALSE to reduce memory footprint).
#' @inheritParams mgcv::bam
#' @inheritDotParams mgcv::bam
#'
#' @rdname fit
#' @export
setGeneric(name = "fit_smooths",
           def = function(sspm_object,
                          family = mgcv::tw,
                          drop.unused.levels = F,
                          method = "fREML",
                          predict = TRUE,
                          keep_fit = TRUE,
                          ...){
             standardGeneric("fit_smooths")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname fit
setMethod(f = "fit_smooths",
          signature(sspm_object = "sspm"),
          function(sspm_object, ...){
            message_not_discrete(sspm_object)
          }
)

#' @export
#' @rdname fit
setMethod(f = "fit_smooths",
          signature(sspm_object = "sspm_discrete"),
          function(sspm_object, family, drop.unused.levels, method, predict, keep_fit, ...){

            # Get all datasets
            datasets <- spm_datasets(sspm_object)

            # Check if all datasets have a mapped formula
            has_no_formulas <- sapply(datasets, function(x){length(spm_formulas(x))<1})
            if(any(has_no_formulas)){
              cli::cli_alert_danger("Not all datasets have mapped formulas.")
              stop("Not all datasets have mapped formulas.", call. = FALSE)
            }

            # Initialize/collect smoothed_data
            full_smoothed_data <- spm_smoothed_data(sspm_object)
            if (is.null(full_smoothed_data)){
              full_smoothed_data <- data.frame()
            }

            # If predict, make the predict matrix from biomass dataset
            if(predict){

              biomass_pos <- which(sapply(datasets, spm_type) == "biomass")
              biomass_dataset <- datasets[[biomass_pos]]
              biomass_data <- spm_data(biomass_dataset)

              min_year <-
                min(as.numeric(as.character(biomass_data[[spm_time_column(biomass_dataset)]])),
                    na.rm = TRUE)
              max_year <-
                max(as.numeric(as.character(biomass_data[[spm_time_column(biomass_dataset)]])),
                    na.rm = TRUE)

              predict_mat <- tidyr::expand_grid(time_col = min_year:max_year,
                                                patch_id = unique(biomass_data$patch_id))

              time_col_biomass <- spm_time_column(biomass_dataset)

            }

            for(dataset in datasets){

              time_col_name <- spm_time_column(dataset)
              predict_mat_tmp <- predict_mat %>%
                dplyr::rename(!!time_col_name := time_col)

              # Get data
              the_data <- spm_data(dataset)

              if(!is_smoothed(dataset)){

                formulas <- spm_formulas(dataset)
                formula_length <- length(formulas)

                if(formula_length == 0){

                  next

                } else {

                  tmp_fit <-
                    vector(mode = "list", length = sum(formula_length))
                  tmp_smoothed <-
                    vector(mode = "list", length = sum(formula_length))

                  for (form_id in seq_len(length.out = length(formulas))){

                    # Index formula
                    form <- formulas[[form_id]]
                    form_name <- paste0(spm_name(dataset), "_f", form_id)
                    form_vars <- formula_vars(form)

                    # Inject name
                    names(tmp_fit)[form_id] <- form_name
                    names(tmp_smoothed)[form_id] <- form_name

                    # Print info
                    cli::cli_alert_info(
                      paste0(" Fitting formula: ",
                             cli::col_yellow(format_formula(raw_formula(form))),
                             " for dataset ", cli::col_cyan(paste0("'", spm_name(dataset),"'"))))

                    # Modify formula env, best solution for now
                    form_env <- attr(translated_formula(form), ".Environment")
                    for(var in names(form_vars)){
                      assign(x = var, value = form_vars[[var]], envir = form_env)
                    }

                    # Fit the formula, important to attach the vars
                    tmp_fit[[form_name]] <-
                      mgcv::bam(formula = translated_formula(form),
                                data = the_data,
                                family = family,
                                drop.unused.levels = drop.unused.levels,
                                method = method,
                                ...)

                    # tmp_df <- data.frame(tmp_fit[[form_name]]$fitted) %>%
                    #   dplyr::bind_cols(ID = the_data[[spm_unique_ID(dataset)]])
                    # names(tmp_df) <- c(form_name, spm_unique_ID(dataset))
                    # tmp_smoothed[[form_name]] <- tmp_df
                  }

                  # Store results at dataset level
                  if(keep_fit){
                    # spm_smoothed_data(datasets[[spm_name(dataset)]]) <- tmp_smoothed
                    spm_smoothed_fit(datasets[[spm_name(dataset)]]) <- tmp_fit
                  }

                  # Predict and store smoothed data to sspm level
                  if(predict){

                    preds <- predict(tmp_fit[[form_name]], predict_mat_tmp, type = "response")
                    column_name <- paste0(spm_name(dataset), "_smooth")
                    preds_df <- predict_mat_tmp %>%
                      dplyr::mutate(!!column_name := as.vector(preds)) %>%
                      dplyr::arrange(!!time_col_name) %>%
                      dplyr::group_by(patch_id) # %>%

                    # TODO finish calculating the change
                    # dplyr::mutate(!!paste0(spm_name(dataset), "_diff") :=
                    #                 log(.[[spm_name(dataset)]]) -
                    #                 log(lag(.[[spm_name(dataset)]])))

                    if (nrow(full_smoothed_data) == 0){

                      full_smoothed_data <- preds_df %>%
                        dplyr::left_join(spm_patches(sspm_object), by = c("patch_id"))

                    } else {

                      preds_df <- preds_df %>%
                        dplyr::rename(!!time_col_biomass := !!time_col_name)
                      full_smoothed_data <- full_smoothed_data %>%
                        dplyr::left_join(preds_df, by = c("patch_id", time_col_biomass))

                    }

                  }
                }

                is_smoothed(datasets[[spm_name(dataset)]]) <- TRUE

              }
            }

            spm_datasets(sspm_object) <- datasets

            full_smoothed_data_clean <- full_smoothed_data %>%
              dplyr::relocate(names(spm_boundaries(sspm_object)),
                              .after = dplyr::last_col()) %>%
              dplyr::relocate(dplyr::contains("smooth")) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(row_ID = 1:nrow(.)) %>%
              dplyr::relocate(row_ID)

            spm_smoothed_data(sspm_object) <-
              new("sspm_data",
                  data = st_as_sf(full_smoothed_data_clean),
                  name = "smoothed_data",
                  type = "smoothed_data",
                  time_column = spm_time_column(spm_datasets(sspm_object,
                                                             "biomass")),
                  uniqueID = "row_ID",
                  coords = NULL,
                  is_smoothed = TRUE)

            # For now return a summary of the fit
            return(sspm_object)

          }
)
