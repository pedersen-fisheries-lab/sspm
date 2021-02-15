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
#' @slot uniqueID **\[character\]** The column of `data` that is unique for all
#'     rows of the data matrix.
#' @slot coords **\[character\]** The column of `data` for longitude and
#'     latitude of the observations.
#' @slot representation **\[character\]** Used internally and for print methods,
#'     encodes the type of dataset.
#'
#' @name spaspm_data-class
#' @rdname spaspm_data-class
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

#' SPASPM smooth object
#'
#' This class is a wrapper around the `xxx.smooth.spec` classes in [mgcv][mgcv].
#' These S3 classes represents all possibe smooths that can be fitted in the
#' package. It is used internally to formally cast users inputs for smooths
#' specifications. It is not directly intended for the user to instantiate
#' objects, but it is still exported.
#'
#' @slot representation **\[character\]** A name for the way the user specified
#'     the smooths input.
#' @slot smooth **\[xxx.smooth.spec\]** An object of class `xxx.smooth.spec`.
#'
#' @seealso See the `mgcv` function for defining smooths: [s()][mgcv::s].
#'
#' @name spaspm_smooth-class

check_spaspm_smooth_class <- function(object){
  checked_rep <- checkmate::test_character(object@representation)
  if(checked_rep){
    checked_smooth  <- grepl("smooth.spec", class(object@smooth), fixed = TRUE)
    if(checked_smooth){
      return(TRUE)
    } else {
      cli::cli_alert_danger("Invalid smooth object")
      return("smooth object must be of class `xxx.smooth.spec`")
    }
  } else {
    cli::cli_alert_danger("Invalid smooth object")
    return("smooth object name must be of class character")
  }
}

#' @describeIn spaspm_smooth-class TODO
setClass("spaspm_smooth",
         slots = list(representation = "character",
                      smooth = "ANY"),
         validity = check_spaspm_smooth_class
)

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
