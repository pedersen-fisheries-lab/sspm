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

            browser()

            # Get all datasets
            datasets <- spm_datasets(sspm_object)

            has_formulas <- sapply(datasets, function(x){
              length(spm_formulas(x)) > 0
            })

            all_fit <-
              vector(mode = "list", length = sum(has_formulas))

            names(all_fit) <- names(datasets)[has_formulas]

            for(dataset in datasets){

              formulas <- spm_formulas(dataset)

              if(length(formulas) == 0){
                next
              }

              all_fit[[spm_name(dataset)]] <-
                vector(mode = "list", length = length(formulas))

              for (form_id in seq_len(length.out = length(formulas))){

                # Index formula
                form <- formulas[[form_id]]

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
                all_fit[[spm_name(dataset)]][[form_id]] <-
                  mgcv::bam(formula = translated_formula(form),
                            data = the_data,
                            family = family,
                            drop.unused.levels = drop.unused.levels,
                            method = method,
                            ...)

              }
            }

            # For now return a summary of the fit
            return(lapply(all_fit, summary))

          }
)
