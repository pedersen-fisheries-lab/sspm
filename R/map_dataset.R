#' Map dataset onto a discretized SPASPM object
#'
#' After discretizing, the next step is to map coavriate datasets.
#'
#' @param spaspm_object **\[spaspm_discrete\]** An object of class
#'    [spaspm_discrete][spaspm_discrete-class].
#' @inheritParams as_spaspm_data
#' @inheritDotParams as_spaspm_data
#'
#' @return
#' The updated object, of class [spaspm_discrete][spaspm_discrete-class].
#'
#' @export
setGeneric(name = "map_dataset",
           def = function(spaspm_object, data, ...){
             standardGeneric("map_dataset")
           }
)

# Methods -----------------------------------------------------------------

# Have to duplicate code here because setting class union between data.frame
# and sf is not possible as `sf` is not exported.

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    data = "data.frame"),
          function(spaspm_object, data, ...){

            updated_object <- cast_and_return(spaspm_object, data, ...)
            return(updated_object)

          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    data = "sf"),
          function(spaspm_object, data, ...){

            updated_object <- cast_and_return(spaspm_object, data, ...)
            return(updated_object)

          }
)

# Helper for the two methods above
cast_and_return <- function(spaspm_object, data, ...){
  # Cast data.frame as spaspm_data
  spaspm_data <- as_spaspm_data(data = data, ...)

  # Call next method
  mapped_objects <- map_dataset(spaspm_object = spaspm_object,
                                data = spaspm_data, ...)
  return(mapped_objects)
}


#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    data = "spaspm_data"),
          function(spaspm_object, data, ...){

            # Append to list of mapped_datasets
            mapped_tmp <- spm_mapped_datasets(spaspm_object)
            all_names <- unlist(c(lapply(mapped_tmp, spm_name), spm_name(data)))

            mapped_tmp <- append(mapped_tmp, list(data))
            names(mapped_tmp) <- all_names

            spm_mapped_datasets(spaspm_object) <- mapped_tmp

            # Return updated spaspm_discretized
            return(spaspm_object)
          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    data = "list"),
          function(spaspm_object, data, ...){

            # If list is provided, lapply or for loop depending on case
            # if ()

          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm"),
          function(spaspm_object, ...){
            message_not_discrete(spaspm_object)
          }
)
