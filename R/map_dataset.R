#' Map dataset onto a discretized sspm object
#'
#' After discretizing, the next step is to map coavriate datasets.
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'    [sspm_discrete][sspm_discrete-class].
#' @inheritParams as_sspm_data
#' @inheritDotParams as_sspm_data
#'
#' @return
#' The updated object, of class [sspm_discrete][sspm_discrete-class].
#'
#' @export
setGeneric(name = "map_dataset",
           def = function(sspm_object, data, ...){
             standardGeneric("map_dataset")
           }
)

# Methods -----------------------------------------------------------------

# Have to duplicate code here because setting class union between data.frame
# and sf is not possible as `sf` is not exported.

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm_discrete",
                    data = "data.frame"),
          function(sspm_object, data, ...){

            updated_object <- cast_and_return(sspm_object, data, ...)
            return(updated_object)

          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm_discrete",
                    data = "sf"),
          function(sspm_object, data, ...){

            updated_object <- cast_and_return(sspm_object, data, ...)
            return(updated_object)

          }
)

# Helper for the two methods above
cast_and_return <- function(sspm_object, data, ...){
  # Cast data.frame as sspm_data
  sspm_data <- as_sspm_data(data = data, ...)

  # Call next method
  mapped_objects <- map_dataset(sspm_object = sspm_object,
                                data = sspm_data, ...)
  return(mapped_objects)
}


#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm_discrete",
                    data = "sspm_data"),
          function(sspm_object, data, ...){

            # Append to list of mapped_datasets
            mapped_tmp <- spm_mapped_datasets(sspm_object)
            all_names <- unlist(c(lapply(mapped_tmp, spm_name), spm_name(data)))

            mapped_tmp <- append(mapped_tmp, list(data))
            names(mapped_tmp) <- all_names

            spm_mapped_datasets(sspm_object) <- mapped_tmp

            # Return updated sspm_discretized
            return(sspm_object)
          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm_discrete",
                    data = "list"),
          function(sspm_object, data, ...){

            browser()

            # Capture args
            args <- list(...)

            # Make checks on length of the name argument
            # must of the same length than data, and unique values
            if(length(args$name) != length(data) | length(unique(args$name)) != length(data)){
              stop(paste0("Argument 'name' is of length ", length(args$name),
                          ", but should be a unique set of the same length than data."),
                   call. = FALSE)
            }

            # Make checks on coords, either a character vector or a list
            if(test_class(args$coords, "character")){
              if(length(args$coords) == 2){
                args$coords <- list(args$coords)
              } else{
                stop("Argument 'coords' shoulb be of a vector of length 2.",
                     call. = FALSE)
              }
            } else if (test_class(args$coords, "list")){
              if(length(args$coords) != 1) {
                if(length(args$coords) != length(data)){
                  stop("Argument 'coords' should be a list of the same length than data.",
                       call. = FALSE)
                }
              }
              if(any(sapply(args$coords, length) != 2)){
                stop("Argument 'coords' should be a list of elements of length 2.",
                     call. = FALSE)
              }
            }

            # Verify correct length of args
            for (arg_id in seq_len(length.out = length(args))){
              if(length(args[[arg_id]]) == length(data)){
                next
              } else if (length(args[[arg_id]]) == 1){
                args[[arg_id]] <- rep(args[[arg_id]], length(data))
              } else{
                stop(paste0("Argument '", names(args[arg_id]), "' is of length ",
                            length(args[[arg_id]]), ". Must be of length 1 or of the same length than data."),
                     call. = FALSE)
              }
            }

            tmp_sspm_discrete <- sspm_object
            for (dat_id in seq_len(length.out = length(data))){
              args_to_pass <- lapply(args, dplyr::nth, dat_id)
              args_to_pass$data <- data[[dat_id]]
              args_to_pass$sspm_object <- tmp_sspm_discrete
              tmp_sspm_discrete <- do.call(map_dataset, args = args_to_pass)
            }

            return(tmp_sspm_discrete)
          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm"),
          function(sspm_object, ...){
            message_not_discrete(sspm_object)
          }
)
