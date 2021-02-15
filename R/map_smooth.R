#' Specify a smoothing model for a discrete `spaspm` object
#'
#' Allows to specify a smoothing model for a discrete `spaspm` object.
#'
#' @param time TODO
#' @param space TODO
#' @param space_time TODO
#'
#' @return
#' The updated object, of class [spaspm_discrete][spaspm_discrete-class].
#'
#' @export
setGeneric(name = "map_smooth",
           def = function(spaspm_object, time, space, space_time,
                          ...){
             standardGeneric("map_smooth")
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
