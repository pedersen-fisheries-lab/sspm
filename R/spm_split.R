#' Split data in test and train sets
#'
#' Split data before fitting spm (WIP).
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'     [sspm][sspm-class] or [sspm_discrete][sspm_discrete-class]
#' @param predicate **\[expression\]** Expression to evaluate to split data.
#'
#' @return
#' The updated sspm object.
#'
#' @export
setGeneric(name = "spm_split",
           def = function(sspm_object,
                          predicate){
             standardGeneric("spm_split")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_split
setMethod(f = "spm_split",
          signature(sspm_object = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @export
#' @rdname spm_split
setMethod(f = "spm_split",
          signature(sspm_object = "sspm_discrete",
                    predicate = "missing"),
          function(sspm_object, predicate){
            stop("Predicate missing.")
          }
)

#' @export
#' @rdname spm_split
setMethod(f = "spm_split",
          signature(sspm_object = "sspm_discrete"),
          function(sspm_object, predicate){

            # Check correct dataset name
            smoothed_data <- spm_smoothed_data(sspm_object)

            # Check dataset is smoothed
            if(is.null(smoothed_data)){
              stop("No smoothed data, splitting scheme cannot be assigned.")
            }

            # TODO add check if splitted

            the_data <- spm_data(smoothed_data)
            selection <- rlang::eval_tidy(rlang::enexpr(predicate),
                                          data = the_data)
            the_data$train_test <- selection
            spm_data(smoothed_data) <- the_data

            smoothed_data@is_splitted <- TRUE

            spm_datasets(sspm_object) <- all_datasets

            return(sspm_object)
          }
)
