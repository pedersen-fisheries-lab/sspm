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

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    dataset = "data.frame"),
          function(spaspm_object, dataset, ...){

            # Cast data.frame as spaspm_data
            spaspm_data <- as_spaspm_data(data = dataset, ...)

            # Call next method
            map_dataset(spaspm_object = spaspm_object,
                        dataset = spaspm_data, ...)
          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    dataset = "spaspm_data"),
          function(spaspm_object, dataset, ...){

            # Append to list of mapped_datasets
            mapped_tmp <- spm_mapped_datasets(spaspm_object)
            mapped_tmp <- append(mapped_tmp, dataset)

            # Return updated spaspm_discretized
            updated_discretized

          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    dataset = "list"),
          function(spaspm_object, dataset, ...){

            # identify if list of data.frames OR
            # list of spaspm_data,
            # lapply acoordingly

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
