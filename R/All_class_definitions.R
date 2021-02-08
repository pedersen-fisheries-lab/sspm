# Imports -----------------------------------------------------------------

#' @import sf cli
#' @importFrom rlang .data
#' @importFrom methods new show
#' @importFrom graphics par

# OldClasses --------------------------------------------------------------

setOldClass("data.frame")
setOldClass("sf")

# ClassUnions -------------------------------------------------------------

setClassUnion("characterOrNULL", c("character", "NULL"))

# -------------------------------------------------------------------------

#' SPASPM dataset structure
#'
#' The first step in the `spaspm` workflow is to register the base dataset
#' (usually biomass) using the [spaspm] function.
#'
#' @slot name **\[character\]** The name of the dataset, default to "Biomass".
#' @slot data **\[data.frame OR sf\]** The dataset.
#' @slot uniqueID **\[character\]** The column of `data` that is unique for all
#'     rows of the data matrix.
#' @slot coords **\[character\]** The column of `data` for longitude and
#'     latitude of the observations.
#' @slot representation **\[character\]** Used internally and for print methods,
#'     encodes the type of dataset.
#'
setClass("spaspm_data",
         slots = list(name = "character",
                      data = "ANY",
                      uniqueID = "character",
                      coords = "characterOrNULL",
                      representation = "character"),
         prototype = prototype(name = "Biomass"),
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
#' The different model classes follow the typical workflow of `spaspm`:
#'  * **`spaspm`** Basic model object.
#'  * **`spaspm_discrete`** Discretized model object. Contains a
#'  [discretization_method][discretization_method-class] object. It can also
#'  containes "mapped datasets" (for example, predator or observator data).
#'
#' @slot name **\[character\]** Name of the model.
#' @slot data **\[[spaspm_data][spaspm_data-class]\]** Observationnal data.
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#'
#' @slot method **\[[discretization_method][discretization_method-class]\]**
#'     *(if discrete)* discretization method used.
#' @slot patches **\[sf\]** *(if discrete)* Patches resulting from
#'     discretization.
#' @slot points **\[sf\]** *(if discrete)* Sample points used for
#'     discretization.
#' @slot mapped_datasets **\[list\]** *(if discrete)* List of
#'     [spaspm_data][spaspm_data-class] objects that are mapped ontp the
#'     base dataset.
#'
setClass("spaspm",
         slots = list(name = "character",
                      data = "spaspm_data",
                      boundaries = "sf"),
         prototype = prototype(name = "Default Model Name")
)

setClass("spaspm_discrete",
         slots = list(method = "discretization_method",
                      patches = "sf",
                      points = "sf",
                      mapped_datasets = "list"),
         prototype = prototype(name = "Default Model Name",
                          mapped_datasets = list()),
         contains = c("spaspm"))

# -------------------------------------------------------------------------

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
