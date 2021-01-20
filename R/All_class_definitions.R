#' @import sf
#' @import checkmate

# Class Defs --------------------------------------------------------------

# Main model class
setClass("spaspm",
         slots = list(name = "character",
                      data = "data.frame",
                      boundaries = "sf"),
         prototype = list(name = "Default Model Name")
)

# Discretization method
setClass("discretization_method",
         slots = list(method = "character",
                      fun = 'function',
                      boundaries = "sf",
                      patches = "sf")
)

# Discretized model => spaspm + discretization_method
setClass("spaspm_discrete",
         slots = list(method = "discretization_method"),
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

setClass("voronoi_discretization",
         slots = list(number_of_patches = "numeric"),
         contains = "discretization_method"
)
