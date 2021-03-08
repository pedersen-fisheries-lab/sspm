#' Accessing OR replacing `discretization_method` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [discretization_method][discretization_method-class].
#'
#' @param sspm_object **\[discretization_method\]** An object of class
#'   [discretization_method][discretization_method-class].
#'
#' @inheritParams base::Extract
#'
#' @rdname accessors-methods-discret_method

# Function ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "method_func",
           def = function(sspm_object) standardGeneric("method_func")
)

#' @describeIn accessors-methods-discret_method TODO
#' @export
setMethod("method_func",
          signature("sspm_object" = "discretization_method"),
          function(sspm_object) sspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-discret_method TODO
#' @export
setGeneric(name = "method_func<-",
           def = function(object, value) standardGeneric("method_func<-")
)

#' @describeIn accessors-methods-discret_method TODO
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
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-discret_method TODO
#' @export
setMethod("spm_name", signature("sspm_object" = "discretization_method"),
          function(sspm_object) sspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-discret_method TODO
#' @export
setMethod("spm_name<-",
          signature("object" = "discretization_method"),
          function(object, value){
            object@name <- value
            validObject(object)
            return(object)
          }
)
