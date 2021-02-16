#' Create a `spaspm_smooth` object
#'
#' This function allows to cast different inputs into an object of class
#' [`spaspm_smooth`][spaspm_smooth-class].
#'
#' @param smooth **\[mgcv xxx.smooth.spec object\]** A smooth object from [mgcv].
#' @param dataset_name **\[character\]** The name of the dataset on which to
#'     map the smooth. See also [spaspm_smooth][spaspm_smooth-class].
#' @param type **\[character\]**
#'
#' @return
#' An object of class [`spaspm_smooth`][spaspm_smooth-class].
#'
#' @seealso See the `mgcv` function for defining smooths: [s()][mgcv::s].
#'
#' @export
setGeneric(name = "as_spaspm_smooth",
           def = function(smooth, dataset_name, type, ...){
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
                    dataset_name = "missing"),
          function(smooth, dataset_name, type, ...){
            cli::cli_alert_danger("Required argument `dataset_name` missing or not specified with `dataset_name=`")

          }
)

# Assess mgcv smooth and cast it
#' @describeIn as_spaspm_smooth TODO
#' @export
setMethod(f = "as_spaspm_smooth",
          signature(smooth = "ANY",
                    dataset_name = "character",
                    type = "missing"),
          function(smooth, dataset_name, type, ...){
            if (is_smooth_spec(smooth)){
              the_smooth <- new("spaspm_smooth",
                                representation = get_base_smooth_type(smooth),
                                dataset_name = dataset_name,
                                smooth = smooth)
              return(the_smooth)
            } else{
              cli::cli_alert_danger("Argument `smooth` must be of type `xxx.smooth.spec`")
            }
          }
)

# TODO doesnt work as expected
#' @describeIn as_spaspm_smooth TODO
#' @export
setMethod(f = "as_spaspm_smooth",
          signature(smooth = "ANY",
                    dataset_name = "character",
                    type = "ANY"),
          function(smooth, dataset_name, type, ...){
            cli::cli_alert_danger("Argument `type` cannot be specified when `smooth` is specified")
          }
)

# If type (ICAR, etc...)
#' @describeIn as_spaspm_smooth TODO
#' @export
setMethod(f = "as_spaspm_smooth",
          signature(smooth = "missing",
                    dataset_name = "character",
                    type = "character"),
          function(smooth, dataset_name, type, ...){
            # Check if types
          }
)
