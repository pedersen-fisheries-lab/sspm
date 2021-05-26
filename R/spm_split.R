#' Split data in test and train sets
#'
#' Split data before fitting spm (WIP).
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'     [sspm][sspm-class] or [sspm_discrete][sspm_discrete-class]
#' @param ... **\[expression\]** Expression to evaluate to split data.
#'
#' @return
#' The updated sspm object.
#'
#' @export
setGeneric(name = "spm_split",
           def = function(sspm_object,
                          ...){
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
          signature(sspm_object = "sspm_discrete"),
          function(sspm_object, ...){

            # Check correct dataset name
            smoothed_data <- spm_smoothed_data(sspm_object)
            time_col <- spm_time_column(smoothed_data)

            # Check dataset is smoothed
            if(is.null(smoothed_data)){
              stop("No smoothed data, splitting scheme cannot be assigned.")
            }

            # TODO add check if splitted

            the_data <- spm_data(smoothed_data)

            is_factor <- FALSE
            if(is.factor(the_data[[spm_time_column(smoothed_data)]])){
              is_factor <- TRUE
              the_data <- the_data %>%
                dplyr::mutate(!!time_col := as.numeric(as.character(.data[[time_col]])))
            }

            the_expr <- (match.call(expand.dots = FALSE)$`...`)[[1]]
            selection <- rlang::eval_tidy(the_expr,
                                          data = the_data)
            # selection <- rlang::eval_tidy(str2lang(predicate),
            #                               data = the_data)
            the_data$train_test <- selection
            is_splitted(smoothed_data) <- TRUE

            if(is_factor){
              the_data <- the_data %>%
                dplyr::mutate(!!time_col := as.factor(.data[[time_col]]))
            }

            spm_data(smoothed_data) <- the_data %>%
              dplyr::relocate(.data$train_test, .after = .data$row_ID)
            spm_smoothed_data(sspm_object) <- smoothed_data

            return(sspm_object)
          }
)
