#' @import methods
#' @import sf
#' @import checkmate

# Class Defs --------------------------------------------------------------

# Main model class
setClass("spaspm",
         slots = list(name = "character",
                      data = "data.frame",
                      boundaries = "sf")
)

# Discretization method
setClass("discretization_method",
         slots = list(method = "character",
                      boundaries = "sf",
                      patches = "sf")
)

setClass("voronoi_discretization",
         slots = list(number_of_patches = "numeric"),
         contains = "discretization_method"
)

# Discretized model => spaspm + discretization_method
setClass("spaspm_discrete",
         slots = list(method = "discretization_method"),
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
         contains = "spaspm_discrete"
)
