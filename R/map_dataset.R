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

            # TODO:
            # Note that all datasets need to have the same ... arguments
            # including names
            tmp_sspm_discrete <- sspm_object
            for (dataset in data){
              tmp_sspm_discrete <- map_dataset(sspm_object = tmp_sspm_discrete,
                                               data = dataset,
                                               ...)
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
