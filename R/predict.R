#' Predict with a SPM model
#'
#' Predict using a fitted SPM model on the whole data or on new data
#'
#' @inheritParams map_dataset
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
          signature(sspm_object = "sspm"),
          function(sspm_object, new_data, ...){

            smoothed_data <- spm_smoothed_data(sspm_object)

            if(is.null(new_data)){

              new_data <- spm_data(smoothed_data)

            }

            fits <- spm_smoothed_fit(smoothed_data)
            fits_length <- length(fits)

            fit_tmp <- list()

            for (fit_id in seq_len(length.out = fits_length)){



            }

          }
)
