#' SPASPM Smoothing functions
#'
#' A full spaspm formula contains calls to the smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`.
#'
#' @param type **\[character\]** Type of smooths.
#' @inheritParams map_formula
#'
#' @rdname smooths
#' @export
setGeneric(name = "smooth_time",
           def = function(type = "ICAR",
                          dataset,
                          spaspm_object,
                          ...){
             standardGeneric("smooth_time")
           }
)

#' @export
#' @describeIn smooths TODO
setGeneric(name = "smooth_space",
           def = function(type = "ICAR",
                          dataset,
                          spaspm_object,
                          ...){
             standardGeneric("smooth_space")
           }
)

#' @export
#' @describeIn smooths TODO
setGeneric(name = "smooth_space_time",
           def = function(type = "ICAR",
                          dataset,
                          spaspm_object,
                          ...){
             standardGeneric("smooth_space_time")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn smooths TODO
setMethod(f = "smooth_time",
          signature(type = "ANY",
                    dataset = "character",
                    spaspm_object = "spaspm_discrete"),
          function(type, dataset, spaspm_object, ...){




          }
)
