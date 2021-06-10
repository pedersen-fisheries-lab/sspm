# Imports -----------------------------------------------------------------

#' @import sf cli mgcv
#' @importFrom rlang .data :=
#' @importFrom methods new show validObject slot
#' @importFrom stats terms as.formula
#' @importFrom graphics par

# OldClasses --------------------------------------------------------------

setOldClass("data.frame")
setOldClass("sf")

# ClassUnions -------------------------------------------------------------

setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("missingOrNULL", c("missing", "NULL"))

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

#' sspm boundary structure
#'
#' One of the first step in the `sspm` workflow is to create one or more
#' object(s) of class `sspm_boundary` from an `sf` object.
#'
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot boundary_col **\[character\]** The column of `data` that represents the
#'     spatial boundaries.
#' @slot method **\[[discretization_method][discretization_method-class]\]**
#'     *(if discrete)* discretization method used.
#' @slot patches **\[sf\]** *(if discrete)* Patches resulting from
#'     discretization.
#' @slot points **\[sf\]** *(if discrete)* Sample points used for
#'     discretization.
#'
#' @name sspm_boundary-class
#' @rdname sspm_boundary-class
#'
setClass("sspm_boundary",
         slots = list(boundaries = "sf",
                      boundary_column = "character")
)

#' @describeIn sspm_boundary-class sspm_discrete_boundary
setClass("sspm_discrete_boundary",
         slots = list(method = "discretization_method",
                      patches = "sf",
                      points = "sf"),
         contains = "sspm_boundary"
)

# -------------------------------------------------------------------------

#' sspm dataset structure
#'
#' One of the first step in the `sspm` workflow is to create one or more
#' object(s) of class `sspm_data` from a `data.frame`, `tibble` or `sf` object.
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
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot formulas **\[list\]** *(if discrete)* List of
#'     [sspm_formula][sspm_formula-class] objects that are mapped onto the
#'     base dataset.
#' @slot smoothed_data **\[sf\]** The smoothed data.
#' @slot smoothed_fit **\[list\]** The fit from smoothing the data
#' @slot is_split **\[logical\]** Whether or not this dataset has been splitted.
#' @slot is_mapped **\[logical\]** Whether the dataset has been mapped to
#'     boundaries (used internally).
#'
#' @name sspm_data-class
#' @rdname sspm_data-class
#'
setClass("sspm_data",
         slots = list(name = "character",
                      data = "ANY",
                      type = "character",
                      time_column = "character",
                      coords = "characterOrNULL",
                      uniqueID = "character",
                      boundaries = "sspm_discrete_boundary",
                      formulas = "list",
                      smoothed_data = "ANY",
                      smoothed_fit = "list",
                      is_split = "logical",
                      is_mapped = "logical"),
         prototype = prototype(is_split = FALSE,
                               is_mapped = FALSE,
                               smoothed_data = NULL)
)

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
#' @slot response **\[charatcer\]** The response variable in the formula.
#' @slot is_fitted **\[logical\]** Whether this formula has already been fitted.
#'
#' @seealso See the `mgcv` function for defining smooths: [s()][mgcv::s].
#'
setClass("sspm_formula",
         slots = list(raw_formula = "formula",
                      translated_formula = "formula",
                      vars = "list",
                      type = "character",
                      response = "character",
                      is_fitted = "logical")
)

# -------------------------------------------------------------------------

#' sspm model class
#'
#' The **`sspm`** model object.
#'
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot datasets **\[list\]** *(if discrete)* List of
#'     [sspm_data][sspm_data-class] that define variables in the SPM model.
#' @slot smoothed_data **\[sf\]** The smoothed data.
#'
#' @name sspm-class
#' @rdname sspm-class
setClass("sspm",
         slots = list(datasets = "list",
                      time_column = "character",
                      uniqueID = "character",
                      boundaries = "sspm_discrete_boundary",
                      formulas = "list",
                      smoothed_data = "ANY"),
         prototype = prototype(datasets = list())
)

# -------------------------------------------------------------------------

# TODO sspm_fit object
