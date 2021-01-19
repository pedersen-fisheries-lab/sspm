# Generics ----------------------------------------------------------------

#' @export
setGeneric(name = "spm_boundaries", signature = "x",
           def = function(x) standardGeneric("spm_boundaries")
)

#' @export
setGeneric(name = "spm_data", signature = "x",
           def = function(x) standardGeneric("spm_data")
)

#' @export
setGeneric(name = "spm_name", signature = "x",
           def = function(x) standardGeneric("spm_name")
)

# Methods for package generics --------------------------------------------

#' @export
setMethod(f = "spm_boundaries", signature = c("x" = "spaspm"),
          function(x){
            x@boundaries
          }
)

#' @export
setMethod(f = "spm_data", signature = c("x" = "spaspm"),
          function(x){
            x@data
          }
)

#' @export
setMethod(f = "spm_name", signature = c("x" = "spaspm"),
          function(x){
            x@name
          }
)

# Methods for global generics ---------------------------------------------

# Show method for spaspm object
setMethod("show",
          "spaspm",
          function(object) {
            cat("SPASPM model object \n") ; cat("\n")

            cat("Name:", object@name, "\n") ; cat("\n")

            cat("Data:") ; cat("\n")
            print(head(object@data)) ; cat("\n")

            cat("Boundaries:") ; cat("\n")
            print(head(object@boundaries)) ; cat("\n")

          }
)
