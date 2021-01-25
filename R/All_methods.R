# Generics ----------------------------------------------------------------

#' @export
setGeneric(name = "spm_boundaries",
           def = function(spaspm_object) standardGeneric("spm_boundaries")
)

#' @export
setGeneric(name = "spm_data",
           def = function(spaspm_object) standardGeneric("spm_data")
)

#' @export
setGeneric(name = "spm_name",
           def = function(spaspm_object) standardGeneric("spm_name")
)

#' @export
setGeneric(name = "spm_discret_method",
           def = function(spaspm_object) standardGeneric("spm_discret_method")
)

# Methods for package generics --------------------------------------------

#' @export
setMethod("spm_boundaries", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@boundaries
)

#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@name
)

#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message(paste0("Model object '", spm_name(spaspm_object),
                           "' is not a discrete model"))
            message("See ?spm_discretize for discretization methods")
          }
)

#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@method
)

# Methods for global generics ---------------------------------------------

# Show method for spaspm object
setMethod("show",
          "spaspm",
          function(object) {
            cat("SPASPM model object \n") ; cat("\n")
            cat_basics(object)
          }
)

setMethod("show",
          "spaspm_discrete",
          function(object) {
            cat("SPASPM model object (DISCRETE)\n") ; cat("\n")
            cat_basics(object)
            cat_discrete(object)
          }
)

# TODO finish show method for method object
# setMethod("show",
#           "discretization_method",
#           function(object) {
#           }
# )

# TODO dim should get dims of data and sf if discrete
# setMethod("dim",
#           "spaspm", function(x) length(x@snpid))

# Print helpers -----------------------------------------------------------

cat_basics <- function(object){
  cat("Name:", object@name, "\n") ; cat("\n")

  cat("Data:") ; cat("\n")
  print(head(object@data)) ; cat("\n")

  cat("Boundaries:") ; cat("\n")
  print(head(object@boundaries)) ; cat("\n")
}

cat_discrete <- function(object){
  # TODO replace with a call to a show method for the
  # Discretization method object
  cat("Discretization method :") ; cat("\n")
  print(object@method) ; cat("\n")
}

