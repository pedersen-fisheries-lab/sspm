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

# Discretized model => spaspm + discretization_method
setClass("spaspm_discrete",
         slots = list(method = "discretization_method",
                      call = "formula"),
         contains = c("spaspm"))

# Fitted model => spaspm + discretization_method + has been fitted
setClass("spaspm_gam_fit",
         slots = list(fit = "data.frame",
                      call = "formula"),
         contains = "spaspm_discrete"
)
