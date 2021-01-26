#' SPASPM discretization method class
#'
#' This class encapsulates a name and a method (function) used for
#' discretization.
#'
#' @slot name **\[character\]** Name of the discretization method.
#' @slot method **\[function\]** Function used for discretization.
#'
setClass("discretization_method",
         slots = list(name = "character",
                      method = 'function')
)

# -------------------------------------------------------------------------

#' SPASPM model classes
#'
#' The different model classes follow the typical workflow of `spaspm`:
#'  * `**spaspm**` Basic model object.
#'  * `**spaspm_discrete**` Discretized model object. Contains a
#'  [discretization_method][discretization_method-class] object.
#'
#' @slot name **\[character\]** Name of the model.
#' @slot data **\[data.frame\]** Observationnal data.
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot method **\[[discretization_method][discretization_method-class]\]**
#'     *(if discrete)* discretization method used.
#' @slot patches **\[sf\]** *(if discrete)* Patches resulting from
#'     discretization.
#' @slot points **\[sf\]** *(if discrete)* Sample points used for
#'     discretization.
#'
setClass("spaspm",
         slots = list(name = "character",
                      data = "data.frame",
                      boundaries = "sf"),
         prototype = list(name = "Default Model Name")
)

setClass("spaspm_discrete",
         slots = list(method = "discretization_method",
                      patches = "sf",
                      points = "sf"),
         prototype = list(name = "Default Model Name"),
         contains = c("spaspm"))

# -------------------------------------------------------------------------

# TODO finish documenting the fit object

# Fitted model => spaspm + discretization_method + has been fitted
setClass("spaspm_gam_fit",
         slots = list(gam_fit = "data.frame",
                      gam_call = "formula"),
         contains = "spaspm_discrete"
)

# Modelled SPM ~ end of workflow
setClass("spaspm_spm_fit",
         slots = list(spm_fit = "data.frame",
                      spm_call = "formula"),
         contains = "spaspm_gam_fit"
)
