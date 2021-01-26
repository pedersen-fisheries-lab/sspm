#' SPASPM model classes
#'
#' @slot name **\[character\]** Name of the model.
#' @slot data **\[data.frame\]** Observationnal data.
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot method **\[discretization_method\]** *(if discrete)* discretization
#'     method used.
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

setClass("discretization_method",
         slots = list(name = "character",
                      method = 'function',
                      boundaries = "sf")
)

#' Dsicrete model test
#' @rdname `spaspm-class` Discretized model
setClass("spaspm_discrete",
         slots = list(method = "discretization_method",
                      patches = "sf",
                      points = "sf"),
         prototype = list(name = "Default Model Name"),
         contains = c("spaspm"))

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

# Subclass Defs -----------------------------------------------------------

# This subclass doensn't work because you need to pass a function that is
# not exported yet. It would work if it was part of a different package.
# setClass("voronoi_discretization",
#          slots = list(number_of_patches = "numeric"),
#          contains = "discretization_method",
#          prototype = list(method = "voronoi_discretization",
#                           fun = tesselate_voronoi))
# )
