#' Fit an SPM model
#'
#' Fit an spm model to a sspm object
#'
#' @param sspm_object **\[sspm_data\]** An object of class
#'     [sspm_data][sspm_data-class].
#' @param formula **\[formula\]** A formula definition of the form
#'     response ~ smoothing_terms + ...
#'
#' @return
#' The updated sspm object.
#'
#' @export
setGeneric(name = "spm",
           def = function(sspm_object,
                          formula){
             standardGeneric("spm")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm
setMethod(f = "spm",
          signature(sspm_object = "sspm",
                    formula = "missing"),
          function(sspm_object, formula){
            cli::cli_alert_danger(" Argument 'formula' missing with no default")
          }
)

#' @export
#' @rdname spm
setMethod(f = "spm",
          signature(sspm_object = "sspm",
                    formula = "formula"),
          function(sspm_object, formula){
          }
)
