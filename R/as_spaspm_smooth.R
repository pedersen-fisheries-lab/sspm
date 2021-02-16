#' Create a `spaspm_smooth` object
#'
#' This function allows to cast different inputs into an object of class
#' [`spaspm_smooth`][spaspm_smooth-class].
#'
#' @param smooth **\[mgcv xxx.smooth.spec object OR character\]** A smooth
#'     object from [mgcv] or a character value from the suppported default types. See
#'     [spm_smooth_types()][spm_smooth_types].
#' @param dataset **\[character\]** The name of the dataset on which to
#'     map the smooth. See also [spaspm_smooth][spaspm_smooth-class].
#' @param dimension **\[character\]** The smoothing dimension the smooth is
#'     applied to: one if "space", "time", "space_time".
#' @param ... Further arguments passed on to build the smooth `TODO`.
#'
#' @return
#' An object of class [`spaspm_smooth`][spaspm_smooth-class].
#'
#' @seealso See the `mgcv` function for defining smooths: [s()][mgcv::s].
#'
#' @export
setGeneric(name = "as_spaspm_smooth",
           def = function(smooth, dataset, dimension, ...){

             if(!checkmate::test_choice(dimension, spm_dimensions())){
               stop(paste0("Dimension must be one of: ",
                           paste0(spm_dimensions(),
                                  collapse =  ", " )))
             }

             standardGeneric("as_spaspm_smooth")
           }
)

# Methods -----------------------------------------------------------------

# If just a smooth object, just return itself
#' @describeIn as_spaspm_smooth TODO
#' @export
setMethod(f = "as_spaspm_smooth",
          signature(smooth = "spaspm_smooth"),
          function(smooth){
            return(smooth)
          }
)

# If name is missing, print error
#' @describeIn as_spaspm_smooth TODO
#' @export
setMethod(f = "as_spaspm_smooth",
          signature(smooth = "ANY",
                    dataset = "missing",
                    dimension = "ANY"),
          function(smooth, dataset, dimension, ...){
            cli::cli_alert_danger("Required argument `dataset` missing or not specified with `dataset =`")
          }
)

# If character, check against values and proceed to next method
#' @describeIn as_spaspm_smooth TODO
#' @export
setMethod(f = "as_spaspm_smooth",
          signature(smooth = "character",
                    dataset = "character",
                    dimension = "character"),
          function(smooth, dataset, dimension, ...){

            if(!checkmate::test_choice(smooth, spm_smooth_types())){
              paste0("Smooth type must be one of: ", paste0(spm_smooth_types(),
                                                            collapse =  ", " ))
            }

            # TODO collect args from ... and check for time/space here for ICAR

            smooth <- dispatch_smooth_type(smooth, dimension, ...)

            smooth_object <-  new("spaspm_smooth",
                                  representation = get_base_smooth_type(smooth),
                                  dataset = dataset,
                                  dimension = dimension,
                                  smooth = smooth)

            return(smooth_object)
          }
)

# If not a character value, assess if mgcv smooth and cast it
#' @describeIn as_spaspm_smooth TODO
#' @export
setMethod(f = "as_spaspm_smooth",
          signature(smooth = "ANY",
                    dataset = "character",
                    dimension = "character"),
          function(smooth, dataset, dimension, ...){
            if (is_smooth_spec(smooth)){
              the_smooth <- new("spaspm_smooth",
                                representation = get_base_smooth_type(smooth),
                                dataset = dataset,
                                dimension = dimension,
                                smooth = smooth)
              return(the_smooth)
            } else{
              cli::cli_alert_danger("Argument `smooth` must be of type `xxx.smooth.spec` or a character value")
            }
          }
)
