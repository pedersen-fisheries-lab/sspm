#' Smooth a variable in a sspm dataset
#'
#' With a formula, smooth a variable in a sspm dataset.
#'
#' @param sspm_object **\[sspm_data\]** An object of class
#'     [sspm_data][sspm_data-class].
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
#' object. This formula makes use of specific smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`. This formula can also contain fixed
#' effects and custom smooths, and can make use of specific smoothing terms
#' `smooth_time()`, `smooth_space()`, `smooth_space_time()`. See Details for
#' more explanations.
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
#' @rdname spm_smooth
setMethod(f = "spm_smooth",
          signature(sspm_object = "sspm_data"),
          function(sspm_object, formula, keep_fit, predict,...){

          }
)
