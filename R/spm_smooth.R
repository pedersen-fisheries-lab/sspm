#' Smooth a variable in a sspm dataset
#'
#' With a formula, smooth a variable in a sspm dataset. See Details for
#' more explanations.
#'
#' @param sspm_object **\[sspm_data\]** An object of class
#'     [sspm_data][sspm_data-class].
#' @param boundary **\[sspm_boundary\]** An object of class
#'     [sspm_boundary][sspm_boundary-class].
#' @param formula **\[formula\]** A formula definition of the form
#'     response ~ smoothing_terms + ...
#' @param keep_fit **\[logical\]** Whether or not to keep the fitted values and
#'   model (default to TRUE, set to FALSE to reduce memory footprint).
#' @param predict **\[logical\]** Whether or not to generate the smoothed
#'   predictions (necessary to fit the final SPM model, default to TRUE).
#' @inheritDotParams mgcv::bam
#'
#' @details
#' This functions allows to specify a model formula for a given discrete sspm
#' object. The formula makes use of specific smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`. The formula can also contain fixed
#' effects and custom smooths, and can make use of specific smoothing terms
#' `smooth_time()`, `smooth_space()`, `smooth_space_time()`.
#'
#' @export
setGeneric(name = "spm_smooth",
           def = function(sspm_object,
                          formula,
                          keep_fit,
                          predict,
                          ...){
             standardGeneric("spm_smooth")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname map_formula
setMethod(f = "spm_smooth",
          signature(sspm_object = "sspm_data",
                    formula = "missing"),
          function(sspm_object, formula, ...){
            cli::cli_alert_danger(" Argument 'formula' missing with no default")
          }
)

#' @export
#' @rdname spm_smooth
setMethod(f = "spm_smooth",
          signature(sspm_object = "sspm_data"),
          function(sspm_object, formula, keep_fit, predict,...){

            # 1. Map bound

            # 1. call map_formula (needs to be modified)

            # 2. call fit with ... arguments

          }
)
