# Imports -----------------------------------------------------------------

#' @import sf cli
#' @importFrom rlang .data
#' @importFrom methods new show validObject
#' @importFrom stats terms as.formula
#' @importFrom graphics par

# OldClasses --------------------------------------------------------------

setOldClass("data.frame")
setOldClass("sf")

# ClassUnions -------------------------------------------------------------

setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("missingOrNULL", c("missing", "NULL"))

# -------------------------------------------------------------------------

#' sspm dataset structure
#'
#' The first step in the `sspm` workflow is to register the base dataset
#' (usually biomass) using the [sspm] function.
#'
#' @slot name **\[character\]** The name of the dataset, default to "Biomass".
#' @slot data **\[data.frame OR sf\]** The dataset.
#' @slot time_column **\[character\]** The column of `data` that represents the
#'     temporal dimension of the dataset.
#' @slot coords **\[character\]** The columns of `data` that represent the
#'     spatial dimension of the dataset: the two columns for longitude and
#'     latitude of the observations.
#' @slot uniqueID **\[character\]** The column of `data` that is unique for all
#'     rows of the data matrix.
#' @slot representation **\[character\]** Used internally and for print methods,
#'     encodes the type of dataset.
#' @slot formulas **\[list\]** *(if discrete)* List of
#'     [sspm_formula][sspm_formula-class] objects that are mapped onto the
#'     base dataset.
#' @slot smoothed **\[Logical\]** Whether or not this dataset has been smoothed.
#'
#' @name sspm_data-class
#' @rdname sspm_data-class
#'
setClass("sspm_data",
         slots = list(name = "character",
                      data = "ANY",
                      time_column = "character",
                      coords = "characterOrNULL",
                      uniqueID = "character",
                      representation = "character",
                      formulas = "list",
                      is_smoothed = "logical"),
         prototype = prototype(name = "Biomass",
                               is_smoothed = FALSE),
         contains = c("sf", "data.frame"))

# TODO reconsider using the stack approach
# setClass("sspm_data_stack",
#          slots = list(stack = "list"))

# -------------------------------------------------------------------------

#' sspm discretization method class
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

#' sspm model classes
#'
#' The different model classes follow the typical workflow of `sspm`:
#'  * **`sspm`** Basic model object.
#'  * **`sspm_discrete`** Discretized model object. Contains a
#'  [discretization_method][discretization_method-class] object. It can also
#'  containes "mapped datasets" (for example, predator or observator data).
#'
#' @slot name **\[character\]** Name of the model.
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot datasets **\[list\]** *(if discrete)* List of
#'     [sspm_data][sspm_data-class] that define variables in the SPM model.
#'
#' @slot method **\[[discretization_method][discretization_method-class]\]**
#'     *(if discrete)* discretization method used.
#' @slot patches **\[sf\]** *(if discrete)* Patches resulting from
#'     discretization.
#' @slot points **\[sf\]** *(if discrete)* Sample points used for
#'     discretization.
#' @slot formulas **\[list\]** *(if discrete)* List of
#'     [sspm_formula][sspm_formula-class] objects that are mapped onto the
#'     base dataset.
#'
#' @name sspm-class
#' @rdname sspm-class
setClass("sspm",
         slots = list(name = "character",
                      datasets = "list",
                      boundaries = "sf"),
         prototype = prototype(name = "My Model",
                               datasets = list())
)

#' @describeIn sspm-class sspm_discrete
setClass("sspm_discrete",
         slots = list(method = "discretization_method",
                      patches = "sf",
                      points = "sf",
                      formulas = "list"),
         prototype = prototype(name = "Default Model Name",
                               formulas = list()),
         contains = c("sspm"))

# -------------------------------------------------------------------------

#' sspm formula object
#'
#' This class is a wrapper around the `formula` class. It is not intended for
#' users to directly manipulate and create new objects.
#'
#' @slot raw_formula **\[formula\]** The raw formula call
#' @slot translated_formula **\[formula\]** The translated formula call ready
#'     to be evaluated.
#' @slot vars **\[list\]** List of relevant variables for the evaluation of the
#'     different smooths.
#' @slot type **\[charatcer\]** One of "smooth" and "surplus", the type of
#'     formula, either for smoothing datasets or for fitting a surplus model
#'
#' @seealso See the `mgcv` function for defining smooths: [s()][mgcv::s].
#'
setClass("sspm_formula",
         slots = list(raw_formula = "formula",
                      translated_formula = "formula",
                      vars = "list",
                      type = "character")
)

# -------------------------------------------------------------------------

# # Fitted model => sspm + discretization_method + has been fitted
# # TODO finish specifying these objects
# setClass("sspm_gam_fit",
#          slots = list(gam_fit = "data.frame",
#                       gam_call = "formula"),
#          contains = c("sspm_discrete", "sspm")
# )
#
# # Modelled SPM ~ end of workflow
# setClass("sspm_spm_fit",
#          slots = list(spm_fit = "data.frame",
#                       spm_call = "formula"),
#          contains = "sspm_gam_fit"
# )
