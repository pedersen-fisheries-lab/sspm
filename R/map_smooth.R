#' Specify a smoothing model for a discrete `spaspm` object
#'
#' Allows to specify a smoothing model for a discrete `spaspm` object.
#'
#' @param spaspm_object **\[spaspm_discrete\]** An object of class
#'    [spaspm_discrete][spaspm_discrete-class].
#' @param time **\[list\]** Specifications for the modeling the temporal
#'    dimension of the smooth model.
#' @param space **\[list\]** Specifications for the modeling the spatial
#'    dimension of the smooth model.
#' @param space_time **\[list\]** Specifications for the modeling the
#'    spatio-temporal dimension of the smooth model.
#' @param ... Other arguments, none used at the moment.
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
