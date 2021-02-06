#' Map dataset onto a discretized SPASPM object
#'
#' Template description
#'
#' @export
setGeneric(name = "map_dataset",
           def = function(spaspm_object, ...){
             standardGeneric("map_dataset")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn ... TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm_discrete"),
          function(spaspm_object, ...){
          }
)

#' @export
#' @describeIn ... TODO
setMethod(f = "map_dataset",
          signature(spaspm_object = "spaspm"),
          function(spaspm_object, ...){
            message_not_discrete(spaspm_object)
          }
)
