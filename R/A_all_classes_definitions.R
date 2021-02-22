# Imports -----------------------------------------------------------------

#' @import sf cli
#' @importFrom rlang .data
#' @importFrom methods new show validObject
#' @importFrom graphics par
#' @importFrom mgcv s

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
#' @slot time_col **\[character\]** The column of `data` that represents the
#'     temporal dimension of the dataset.
#' @slot coords **\[character\]** The columns of `data` that represent the
#'     spatial dimension of the dataset: the two columns for longitude and
#'     latitude of the observations.
#' @slot uniqueID **\[character\]** The column of `data` that is unique for all
#'     rows of the data matrix.
#' @slot representation **\[character\]** Used internally and for print methods,
#'     encodes the type of dataset.
#'
#' @name spaspm_data-class
#' @rdname spaspm_data-class
#'
setClass("spaspm_data",
         slots = list(name = "character",
                      data = "ANY",
                      time_col = "character",
                      coords = "characterOrNULL",
                      uniqueID = "character",
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
#' @name discretization_method-class
#' @rdname discretization_method-class
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
#' @slot mapped_smooths **\[list\]** *(if discrete)* List of mapped smoothed
#'     used to specify a given set of models.
#'
#' @name spaspm-class
#' @rdname spaspm-class
#'
setClass("spaspm",
         slots = list(name = "character",
                      data = "spaspm_data",
                      boundaries = "sf"),
         prototype = prototype(name = "Default Model Name")
)

#' @describeIn spaspm-class spaspm_discrete
setClass("spaspm_discrete",
         slots = list(method = "discretization_method",
                      patches = "sf",
                      points = "sf",
                      mapped_datasets = "list",
                      mapped_smooths = "list"),
         prototype = prototype(name = "Default Model Name",
                               mapped_datasets = list(),
                               mapped_smooths = list()),
         contains = c("spaspm"))

# -------------------------------------------------------------------------

# Fitted model => spaspm + discretization_method + has been fitted
# TODO finish specifying these objects
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
