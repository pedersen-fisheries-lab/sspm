# Package Generics --------------------------------------------------------
# -------------------------------------------------------------------------

# Exported ----------------------------------------------------------------

#' Accessing `spaspm` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [spaspm][spaspm-class] and others derivative classes
#' (`spaspm_discrete`, etc...).
#'
#' @param spaspm_object **\[spaspm OR adjacent\]** An object of class
#'     [spaspm][spaspm-class] or others derivative classes.
#'
#' @rdname accessors
#' @export
setGeneric(name = "spm_boundaries",
           def = function(spaspm_object) standardGeneric("spm_boundaries")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_data",
           def = function(spaspm_object) standardGeneric("spm_data")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_name",
           def = function(spaspm_object) standardGeneric("spm_name")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_discret_method",
           def = function(spaspm_object) standardGeneric("spm_discret_method")
)

# Methods for package generics --------------------------------------------
# -------------------------------------------------------------------------

# Exported ----------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@name
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_boundaries", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@boundaries
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

#' @describeIn accessors TODO
#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@method
)

# Methods for global generics ---------------------------------------------
# -------------------------------------------------------------------------

# Show method for spaspm object
setMethod("show",
          "spaspm",
          function(object) {
            cat("  * SPASPM model object \n")
            cat_model_basics(object)
          }
)

setMethod("show",
          "spaspm_discrete",
          function(object) {
            cat("  * SPASPM model object (DISCRETIZED) \n")
            cat_model_basics(object)
            cat(" \n")
            cat("  * DISCRETIZATION INFO \n")
            show(object@method)
            cat_model_discrete(object)
          }
)

setMethod("show",
          "discretization_method",
          function(object) {
            cat("  Disc. method :", object@name, "\n")
          }
)

# TODO dim should get dims of data and sf if discrete
# setMethod("dim",
#           "spaspm", function(x) length(x@snpid))

# Print helpers -----------------------------------------------------------
# -------------------------------------------------------------------------

cat_model_basics <- function(object){
  cat("  Name         :", object@name, "\n")
  cat("  Data         :", "`data.frame`,",
      dim(object@data)[1], "obs. of", dim(object@data)[2], "variables \n")
  cat("  Boundaries   :", "Simple feature collection with",
      dim(object@boundaries)[1] ,"features and", dim(object@boundaries)[2], "field \n")
}

cat_model_discrete <- function(object){
  cat("  Patches      :", "Simple feature collection with",
      dim(object@patches)[1] ,"features and", dim(object@patches)[2], "field \n")
  cat("  Points       :", "Simple feature collection with",
      dim(object@points)[1] ,"features and", dim(object@points)[2], "field \n")
}

