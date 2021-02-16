#' Specify a smoothing model for a discrete `spaspm` object
#'
#' Allows to specify a smoothing model for a discrete `spaspm` object.
#'
#' @inheritParams map_dataset
#' @inheritParams as_spaspm_smooth
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
           def = function(spaspm_object, dataset = "Biomass",
                          time = NULL, space = NULL, space_time = NULL,
                          ...){
             standardGeneric("map_smooth")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn map_smooth TODO
setMethod(f = "map_smooth",
          signature(spaspm_object = "spaspm"),
          function(spaspm_object, dataset, time, space, space_time, ...){
            message_not_discrete()
          }
)

#' @export
#' @describeIn map_smooth TODO
setMethod(f = "map_smooth",
          signature(spaspm_object = "spaspm_discrete"),
          function(spaspm_object, dataset, time, space, space_time, ...){

            # Start with list
            list_of_smooths <- list(time = time,
                                    space = space,
                                    space_time = space_time)

            # Remove values set to false

            # Remove smooths we don't want to fit
            list_of_smooths <-
              list_of_smooths[lapply(list_of_smooths,
                                     function(x) ifelse(is.logical(x), x, TRUE))]

            # Check the smooths
            smooths_okay <- mapply(smooth_object = list_of_smooths,
                                   smooth_name = names(list_of_smooths),
                                   FUN = check_smooth)

            if(all(smooths_okay)){
              # Assign default smooths
              list_of_smooths[sapply(list_of_smooths, is.null)]<-
                default_smooths(list_of_smooths[sapply(list_of_smooths, is.null)])

            }
          }
)
