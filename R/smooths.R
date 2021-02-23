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

            # Get args from ellipsis add time_col as the first element by appending
            args <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            if(!is.null(type)){
              if(type == "ICAR"){
                args_and_vars <- do.call(ICAR,
                                         append(list(spaspm_object = spaspm_object,
                                                     dataset = dataset,
                                                     dimension = "time"),
                                                args))
              } else {
                stop("Smooth type provided not supported")
              }
            }

            return(args_and_vars)

          }
)
