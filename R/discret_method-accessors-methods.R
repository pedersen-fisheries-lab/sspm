#' Accessing OR replacing `discretization_method` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [discretization_method][discretization_method-class].
#'
#' @param spaspm_object **\[discretization_method\]** An object of class
#'     [discretization_method][discretization_method-class].
#'
#' @rdname discret_method-accessors-methods

# Function ----------------------------------------------------------------

#' @export
setGeneric(name = "method_func",
           def = function(spaspm_object) standardGeneric("method_func")
)

#' @describeIn discret_method-accessors-methods TODO
#' @export
setMethod("method_func",
          signature("spaspm_object" = "discretization_method"),
          function(spaspm_object) spaspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @describeIn discret_method-accessors-methods TODO
#' @export
setGeneric(name = "method_func<-",
           def = function(object, value) standardGeneric("method_func<-")
)

#' @describeIn discret_method-accessors-methods TODO
#' @export
setMethod("method_func<-",
          signature("object" = "discretization_method"),
          function(object, value){
            object@method <- value
            validObject(object)
            return(object)
          }
)

# Name --------------------------------------------------------------------

#' @describeIn discret_method-accessors-methods TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "discretization_method"),
          function(spaspm_object) spaspm_object@name
)

#' @describeIn discret_method-accessors-methods TODO
#' @export
setMethod("spm_name<-",
          signature("object" = "discretization_method"),
          function(object, value){
            object@name <- value
            validObject(object)
            return(object)
          }
)
