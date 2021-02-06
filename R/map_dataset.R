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

            # call as_spaspm_data
            # call next method

          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete",
                    dataset = "spaspm_data"),
          function(spaspm_object, dataset, ...){

            # append to list of mapped_datasets
            # return new spaspm_discretized

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
