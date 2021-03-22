#' Fit the GAM part of a sspm model
#'
#' Once formulas have been mapped onto a sspm discrete object, the GAMs can be
#' fitted with this function. Arguments can be passed onto `bam`.
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'   [sspm_discrete][sspm_discrete-class].
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
                          ...){
             standardGeneric("fit_smooths")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn fit TODO
setMethod(f = "fit_smooths",
          signature(sspm_object = "sspm"),
          function(sspm_object, ...){
            message_not_discrete(sspm_object)
          }
)

#' @export
#' @describeIn fit TODO
setMethod(f = "fit_smooths",
          signature(sspm_object = "sspm_discrete"),
          function(sspm_object, family, drop.unused.levels, method, ...){

            # Get all datasets
            datasets <- spm_datasets(sspm_object)

            has_formulas <- sapply(datasets, function(x){
              length(spm_formulas(x)) > 0
            })

            all_fit <-
              vector(mode = "list", length = sum(has_formulas))
            all_smoothed <-
              vector(mode = "list", length = sum(has_formulas))

            names(all_fit) <- names(datasets)[has_formulas]
            names(all_smoothed) <- names(datasets)[has_formulas]

            for(dataset in datasets){

              formulas <- spm_formulas(dataset)

              if(length(formulas) == 0){
                next
              }

              # all_fit[[spm_name(dataset)]] <-
              #   vector(mode = "list", length = length(formulas))

              for (form_id in seq_len(length.out = length(formulas))){

                # Index formula
                form <- formulas[[form_id]]
                form_name <- paste0(spm_name(dataset), "_f", form_id)

                # Print info
                cli::cli_alert_info(
                  paste0("Fitting formula: ",
                         cli::col_yellow(format_formula(raw_formula(form))),
                         " for dataset ", cli::col_cyan(paste0("'", spm_name(dataset),"'"))))

                # Get data
                the_data <- spm_data(dataset)
                form_vars <- formula_vars(form)

                # Modify formula env, best solution for now
                form_env <- attr(translated_formula(form), ".Environment")
                for(var in names(form_vars)){
                  assign(x = var, value = form_vars[[var]], envir = form_env)
                }

                # Fit the formula, important to attach the vars
                all_fit[[spm_name(dataset)]][[form_name]] <-
                  mgcv::bam(formula = translated_formula(form),
                            data = the_data,
                            family = family,
                            drop.unused.levels = drop.unused.levels,
                            method = method,
                            ...)

                tmp_df <-
                  data.frame(all_fit[[spm_name(dataset)]][[form_name]]$fitted)
                names(tmp_df) <- form_name
                all_smoothed[[spm_name(dataset)]][[form_name]] <-
                  tmp_df
              }
            }

            sspm_discrete_smoothed <-
              new("sspm_discrete_smoothed",
                  discrete = sspm_object,
                  smoothed_data = all_smoothed,
                  gam_fit = all_fit)

            # For now return a summary of the fit
            return(sspm_discrete_smoothed)

          }
)
