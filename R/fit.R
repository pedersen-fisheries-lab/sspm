#' Fit the GAM part of a SPASPM model
#'
#' Once formulas have been mapped onto a SPASPM discrete object, the GAMs can
#' be fitted with this function. Arguments can be passed onto `bam`.
#'
#' @param spaspm_object **\[spaspm_discrete\]** An object of class
#'     [spaspm_discrete][spaspm_discrete-class].
#' @inheritParams mgcv::bam
#' @inheritDotParams mgcv::bam
#'
#' @rdname fit
#' @export
setGeneric(name = "fit_smooths",
           def = function(spaspm_object,
                          family = mgcv::tw,
                          drop.unused.levels = F,
                          method = "REML",
                          ...){
             standardGeneric("fit_smooths")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn fit TODO
setMethod(f = "fit_smooths",
          signature(spaspm_object = "spaspm"),
          function(spaspm_object, ...){
            message_not_discrete(spaspm_object)
          }
)

#' @export
#' @describeIn fit TODO
setMethod(f = "fit_smooths",
          signature(spaspm_object = "spaspm_discrete"),
          function(spaspm_object, family, drop.unused.levels, method, ...){

            # Get the mapped formulas
            mapped_formulas <- spm_mapped_formulas(spaspm_object)

            # Loop through all of them
            all_fit <- vector(mode = "list", length = length(mapped_formulas))
            for (form_id in seq_len(length.out = length(mapped_formulas))){

              # Index formula
              form <- mapped_formulas[[form_id]]

              # Print info
              cli::cli_alert_info(
                paste0("Fitting formula: ", form_id, " out of ", length(mapped_formulas)))
              cli::cli_alert_info(
                paste0("Fitting formula: ",
                       cli::col_yellow(format_formula(raw_formula(form))),
                       " for dataset ", cli::col_cyan(paste0("'", dataset(form),"'"))))

              # Get data
              the_data <- spm_data(spm_datasets(spaspm_object)[[dataset(form)]])
              form_vars <- formula_vars(form)

              # Modify formula env, best solution for now
              form_env <- attr(translated_formula(form), ".Environment")
              for(var in names(form_vars)){
                assign(x = var, value = form_vars[[var]], envir = form_env)
              }

              # Fit the formula, important to attach the vars
              all_fit[[form_id]] <-
                mgcv::bam(formula = translated_formula(form),
                          data = the_data,
                          family = family,
                          drop.unused.levels = drop.unused.levels,
                          method = method,
                          ...)

            }

            # For now return a summary of the fit
            return(lapply(all_fit, summary))

          }
)
