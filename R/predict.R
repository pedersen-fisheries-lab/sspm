#' Predict with a SPM model
#'
#' Predict using a fitted SPM model on the whole data or on new data
#'
#' @inheritParams map_dataset
#' @param new_data **\[data.frame\]**
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
          signature(sspm_object = "sspm_discrete"),
          function(sspm_object, new_data, ...){
          }
)
