#' Split data in test and train sets
#'
#' Split data before fitting spm (WIP).
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'     [sspm][sspm-class] or [sspm_discrete][sspm_discrete-class]
#' @param dataset **\[chacacter\]** The dataset to split.
#' @param predicate **\[expression\]** Expression to evaluate to split data.
#'
#' @return
#' The updated sspm object.
#'
#' @export
setGeneric(name = "spm_split",
           def = function(sspm_object,
                          dataset,
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
                    dataset = "missing"),
          function(sspm_object, dataset, predicate){
            stop("Argument 'dataset' missing")
          }
)

#' @export
#' @rdname spm_split
setMethod(f = "spm_split",
          signature(sspm_object = "sspm_discrete",
                    dataset = "character"),
          function(sspm_object, dataset, predicate){

            # Check correct dataset name
            all_datasets <- spm_datasets(sspm_object)
            all_datasets_names <- sapply(all_datasets, spm_name)

            if(!(dataset %in% all_datasets_names)){
              stop("Wrong dataset name.")
            }

            # Check dataset is smoothed
            if(!is_smoothed(all_datasets[[dataset]])){
              stop("Dataset not smooth")
            }

            # Verify of already splitted

            the_data <- spm_data(all_datasets[[dataset]])
            the_data_smoothed <- spm_smoothed_data(all_datasets[[dataset]])

            selection <- rlang::eval_tidy(rlang::enexpr(predicate),
                                          data = the_data)
            the_data$train_test <- selection
            for (smooth_id in seq_len(length.out = length(the_data_smoothed))){
              df <- the_data_smoothed[[smooth_id]]
              df$train_test <- selection
              the_data_smoothed[[smooth_id]] <- df
            }

            spm_data(all_datasets[[dataset]]) <- the_data
            spm_smoothed_data(all_datasets[[dataset]]) <- the_data_smoothed

            all_datasets[[dataset]]@is_splitted <- TRUE

            spm_datasets(sspm_object) <- all_datasets

            return(sspm_object)
          }
)
