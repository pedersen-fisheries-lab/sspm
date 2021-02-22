#' Map model formula onto a discretized SPASPM object
#'
#' This functions allows to specify a model formula for a given discrete SPASPM
#' object. This formula makes use of specific smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`. This formula can also contain fixed
#' effects and custom smooths, and can make use of specific smoothing terms
#' `smooth_time()`, `smooth_space()`, `smooth_space_time()`. See Details for
#' more explanations.
#'
#'  @param spaspm_object **\[spaspm_discrete\]** An object of class
#'    [spaspm_discrete][spaspm_discrete-class].
#'  @param dataset **\[character\]** The name of the dataset among base and/or
#'    mapped datasets for which to specify the formula
#'  @param formula **\[formula\]** A formula definition of the form
#'    response ~ smoothing_terms + ...
#'  @param ... Further arguments passed down, none used for now.
#'
#' @export
setGeneric(name = "map_formula",
           def = function(spaspm_object,
                          dataset,
                          formula,
                          ...){
             standardGeneric("map_formula")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn map_formula TODO
setMethod(f = "map_formula",
          signature(spaspm_object = "spaspm"),
          function(spaspm_object, ...){
            message_not_discrete()
          }
)
