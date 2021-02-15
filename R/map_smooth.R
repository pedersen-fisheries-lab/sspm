#' Specify a smoothing model for a discrete `spaspm` object
#'
#' Allows to specify a smoothing model for a discrete `spaspm` object.
#'
#' @export
setGeneric(name = "map_smooth",
           def = function(spaspm_object, time, space, space_time,
                          ...){
             standardGeneric("")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn map_smooth TODO
setMethod(f = "map_smooth",
          signature(spaspm_object = "spaspm"),
          function(spaspm_object, time, space, space_time, ...){
            message_not_discrete()
          }
)

#' @export
#' @describeIn map_smooth TODO
setMethod(f = "map_smooth",
          signature(spaspm_object = "spaspm_discrete"),
          function(spaspm_object, time, space, space_time, ...){
            message_not_discrete()
          }
)
