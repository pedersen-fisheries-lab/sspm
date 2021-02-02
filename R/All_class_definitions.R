# Imports -----------------------------------------------------------------

#' @import sf cli
#' @importFrom rlang .data
#' @importFrom methods new show
#' @importFrom graphics par

# OldClasses --------------------------------------------------------------

setOldClass("data.frame")
setOldClass("sf")

# ClassUnions -------------------------------------------------------------

setClassUnion("ANY_coords", c("character", "NULL"))

# -------------------------------------------------------------------------

#' SPASPM dataset structure
#'
#' @slot data **\[ANY\]** TODO
#' @slot uniqueID **\[character\]** TODO
#' @slot is_spatial **\[logical\]** TODO
#' @slot coords **\[character\]** TODO
#' @slot representation **\[character\]** TODO
#'
setClass("spaspm_data",
         slots = list(data = "ANY",
                      uniqueID = "character",
                      is_spatial = "logical",
                      coords = "ANY_coords",
                      representation = "character"),
         contains = c("sf", "data.frame"))

# TODO reconsider using the stack approach
# setClass("spaspm_data_stack",
#          slots = list(stack = "list"))

# -------------------------------------------------------------------------

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
#' TODO updates slots
#'
#' The different model classes follow the typical workflow of `spaspm`:
#'  * **`spaspm`** Basic model object.
#'  * **`spaspm_discrete`** Discretized model object. Contains a
#'  [discretization_method][discretization_method-class] object.
#'
#' @slot name **\[character\]** Name of the model.
#' @slot data **\[[spaspm_data][spaspm_data-class]\]** Observationnal data.
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#'
#' @slot data_spatial **\[sf\]** *(if discrete)* Spatial version of `data`,
#'     resulting from discretization.
#' @slot method **\[[discretization_method][discretization_method-class]\]**
#'     *(if discrete)* discretization method used.
#' @slot patches **\[sf\]** *(if discrete)* Patches resulting from
#'     discretization.
#' @slot points **\[sf\]** *(if discrete)* Sample points used for
#'     discretization.
#' @slot mapped_datasets **\[list\]** *(if discrete)* List of
#'     [spaspm_data][spaspm_data-class] objects.
#'
setClass("spaspm",
         slots = list(name = "character",
                      data = "spaspm_data",
                      boundaries = "sf"),
         prototype = list(name = "Default Model Name")
)

setClass("spaspm_discrete",
         slots = list(method = "discretization_method",
                      patches = "sf",
                      points = "sf",
                      mapped_datasets = "list"),
         prototype = list(name = "Default Model Name"),
         contains = c("spaspm"))

# -------------------------------------------------------------------------

# TODO finish documenting the fit object

# Fitted model => spaspm + discretization_method + has been fitted
setClass("spaspm_gam_fit",
         slots = list(gam_fit = "data.frame",
                      gam_call = "formula"),
         contains = c("spaspm_discrete", "spaspm")
)

# Modelled SPM ~ end of workflow
setClass("spaspm_spm_fit",
         slots = list(spm_fit = "data.frame",
                      spm_call = "formula"),
         contains = "spaspm_gam_fit"
)
