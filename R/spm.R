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
                          formula,
                          ...){
             standardGeneric("spm")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm
setMethod(f = "spm",
          signature(sspm_object = "sspm",
                    formula = "missing"),
          function(sspm_object, formula, ...){
            cli::cli_alert_danger(" Argument 'formula' missing with no default")
          }
)

#' @export
#' @rdname spm
setMethod(f = "spm",
          signature(sspm_object = "sspm",
                    formula = "formula"),
          function(sspm_object, formula, ...){

            # 1. Is there a splitting scheme?
            if(!is_split(sspm_object)){
              stop("Data must be split with a test/train column.")
            } else {
              old_data <- spm_smoothed_data(sspm_object)
              train_data <- old_data %>%
                dplyr::filter(.data$train_test == TRUE)
              spm_smoothed_data(sspm_object) <- train_data
            }

            # 1. call map_formula
            data_frame <- spm_smoothed_data(sspm_object)
            time_column <- spm_time_column(sspm_object)
            boundaries <- spm_boundaries(sspm_object)

            browser()

            # Pass onto the sspm_data method
            sspm_formula <- map_formula(data_frame = data_frame,
                                        boundaries = boundaries,
                                        formula = formula,
                                        time_column = time_column,
                                        ...)

            spm_smoothed_data(sspm_object) <- old_data

            # 2. call fit with ... arguments
            # sspm_object_fitted <- sspm_object_joined %>%
            #   fit_smooths(boundaries = boundaries,
            #               keep_fit = keep_fit, predict = predict, ...)

            return(sspm_object_fitted)

          }
)
