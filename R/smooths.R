#' SPASPM Smoothing functions
#'
#' A full spaspm formula contains calls to the smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`.
#'
#' @param type **\[character\]** Type of smooths.
#'
#' @rdname smooths
#' @export
setGeneric(name = "smooth_time",
           def = function(type){
             standardGeneric("smooth_time")
           }
)

#' @export
#' @describeIn smooths TODO
setGeneric(name = "smooth_space",
           def = function(type){
             standardGeneric("smooth_space")
           }
)

#' @export
#' @describeIn smooths TODO
setGeneric(name = "smooth_space_time",
           def = function(type){
             standardGeneric("smooth_space_time")
           }
)

# # Methods -----------------------------------------------------------------
#
# #' @export
# #' @describeIn ... TODO
# setMethod(f = "",
#           signature(spaspm_object = ""),
#           function(spaspm_object, ...){
#           }
# )
