#' Predict with a SPM model
#'
#' Predict using a fitted SPM model on the whole data or on new data
#'
#' @inheritParams spm_smooth
#' @param new_data **\[data.frame\]**
#' @param ... Arguments passed on to [predict.bam].
#'
#' @export
setGeneric(name = "spm_predict",
           def = function(sspm_object,
                          new_data = NULL,
                          ...){
             standardGeneric("spm_predict")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict",
          signature(sspm_object = "sspm_fit",
                    new_data = "data.frame"),
          function(sspm_object, new_data, ...){
            new_data <- as.list(new_data)
            spm_predict(sspm_object = sspm_object, new_data = new_data, ...)
          }
)

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict",
          signature(sspm_object = "sspm_fit",
                    new_data = "missing"),
          function(sspm_object, new_data, ...){
            new_data <- append(as.list(spm_smoothed_data(sspm_object)),
                               formula_vars(spm_formulas(sspm_model_fit)))
            spm_predict(sspm_object = sspm_object, new_data = new_data, ...)
          }
)

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict",
          signature(sspm_object = "sspm_fit",
                    new_data = "list"),
          function(sspm_object, new_data, ...){

            preds <- spm_get_fit(sspm_model_fit) %>%
              predict.bam(newdata = new_data, ...)

            return(preds)
          }
)
