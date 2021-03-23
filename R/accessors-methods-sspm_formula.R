#' Accessing OR replacing `sspm_formula` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [sspm_formula][sspm_formula-class].
#'
#' @param sspm_object **\[sspm_formula\]** An object of class
#'     [sspm_formula][sspm_formula-class].
#'
#' @inheritParams base::Extract
#'
#' @rdname accessors-methods-sspm_formula

# Raw formula -------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "raw_formula",
           def = function(sspm_object) standardGeneric("raw_formula")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("raw_formula",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@raw_formula
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "raw_formula<-",
           def = function(object, value) standardGeneric("raw_formula<-")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("raw_formula<-",
          signature("object" = "sspm_formula"),
          function(object, value){
            object@raw_formula <- value
            validObject(object)
            return(object)
          }
)

# Translated_formula ------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "translated_formula",
           def = function(sspm_object) standardGeneric("translated_formula")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("translated_formula",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@translated_formula
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "translated_formula<-",
           def = function(object, value) standardGeneric("translated_formula<-")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("translated_formula<-",
          signature("object" = "sspm_formula"),
          function(object, value){
            object@translated_formula <- value
            validObject(object)
            return(object)
          }
)

# Dataset -----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "dataset",
           def = function(sspm_object) standardGeneric("dataset")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("dataset",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@dataset
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "dataset<-",
           def = function(object, value) standardGeneric("dataset<-")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("dataset<-",
          signature("object" = "sspm_formula"),
          function(object, value){
            object@dataset <- value
            validObject(object)
            return(object)
          }
)

# Vars --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "formula_vars",
           def = function(sspm_object) standardGeneric("formula_vars")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("formula_vars",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@vars
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "formula_vars<-",
           def = function(object, value) standardGeneric("formula_vars<-")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("formula_vars<-",
          signature("object" = "sspm_formula"),
          function(object, value){
            object@vars <- value
            validObject(object)
            return(object)
          }
)

# Type --------------------------------------------------------------------

# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "formula_type",
           def = function(sspm_object) standardGeneric("formula_type")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("formula_type",
          signature("sspm_object" = "sspm_formula"),
          function(sspm_object) sspm_object@type
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setGeneric(name = "formula_type<-",
           def = function(object, value) standardGeneric("formula_type<-")
)

#' @describeIn accessors-methods-sspm_formula TODO
#' @export
setMethod("formula_type<-",
          signature("object" = "sspm_formula"),
          function(object, value){
            object@type <- value
            validObject(object)
            return(object)
          }
)
