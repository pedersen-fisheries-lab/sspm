#' Create a `spaspm_smooth` object
#'
#' This function allows to cast different inputs into an object of class
#' [`spaspm_smooth`][spaspm_smooth-class].
#'
#' @param smooth **\[mgcv xxx.smooth.spec object\]** A smooth object from [mgcv].
#' @param dataset_name **\[character\]** The name of the dataset on which to
#'     map the smooth. See also [spaspm_smooth][spaspm_smooth-class].
#' @param type **\[character\]**
#' @param args **\[list\]**
#'
#' @return
#' An object of class [`spaspm_smooth`][spaspm_smooth-class].
#'
#' @export
setGeneric(name = "as_spaspm_smooth",
           def = function(smooth, type, args){
             standardGeneric("as_spaspm_smooth")
           }
)

# Methods -----------------------------------------------------------------

#' @describeIn as_spaspm_smooth TODO
#' @export
setMethod(f = "as_spaspm_smooth",
          signature(smooth = "spaspm_smooth"),
          function(smooth){
            return(smooth)
          }
)
