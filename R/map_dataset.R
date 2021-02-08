#' Map dataset onto a discretized SPASPM object
#'
#' TODO
#'
#' @export
setGeneric(name = "map_dataset",
           def = function(spaspm_object, dataset, ...){
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
                    dataset = "data.frame"),
          function(spaspm_object, dataset, ...){

            updated_object <- cast_and_return(spaspm_object, dataset, ...)
            return(updated_object)

          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    dataset = "sf"),
          function(spaspm_object, dataset, ...){

            updated_object <- cast_and_return(spaspm_object, dataset, ...)
            return(updated_object)

          }
)

# Helper for the two methods above
cast_and_return <- function(spaspm_object, dataset, ...){
  # Cast data.frame as spaspm_data
  spaspm_data <- as_spaspm_data(data = dataset, ...)

  # Call next method
  mapped_objects <- map_dataset(spaspm_object = spaspm_object,
                                dataset = spaspm_data, ...)
  return(mapped_objects)
}


#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    dataset = "spaspm_data"),
          function(spaspm_object, dataset, ...){

            # Append to list of mapped_datasets
            mapped_tmp <- spm_mapped_datasets(spaspm_object)
            all_names <- unlist(c(lapply(mapped_tmp, names), spm_name(dataset)))

            mapped_tmp <- append(mapped_tmp, list(dataset))
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
                    dataset = "list"),
          function(spaspm_object, dataset, ...){

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
