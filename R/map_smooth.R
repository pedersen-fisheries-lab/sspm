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
           def = function(spaspm_object, dataset_name = "Biomass",
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
          function(spaspm_object, dataset_name, time, space, space_time, ...){
            message_not_discrete()
          }
)

#' @export
#' @describeIn map_smooth TODO
setMethod(f = "map_smooth",
          signature(spaspm_object = "spaspm_discrete"),
          function(spaspm_object, dataset_name, time, space, space_time, ...){

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

# This function is responsible for checking the inputs of each time, space
# and space_time smooth. It will return TRUE or send an error message
check_smooth <- function(smooth_object, smooth_name){

  if(is.null(smooth_object) | is.list(smooth_object)){
    return_value <- TRUE
  } else {
    the_class <- class(smooth_object)
    checked  <- grepl("smooth.spec", the_class, fixed = TRUE)

    if(!checked){
      cli::cli_alert_danger(paste0("Specifications for '",
                                   smooth_name,
                                   "' smooth are incorrect"))
      return_value <- FALSE

    } else {
      return_value <- TRUE
    }
  }

  return(return_value)
}

# This functions gets a named list of the missing smooths and return default
# smooths
