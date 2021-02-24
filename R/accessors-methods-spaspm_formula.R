#' Accessing OR replacing `spaspm_formula` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [spaspm_formula][spaspm_formula-class].
#'
#' @param spaspm_object **\[spaspm_formula\]** An object of class
#'     [spaspm_formula][spaspm_formula-class].
#'
#' @inheritParams base::Extract
#'
#' @rdname accessors-methods-spaspm_formula

# Raw formula -------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "raw_formula",
           def = function(spaspm_object) standardGeneric("raw_formula")
)

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setMethod("raw_formula",
          signature("spaspm_object" = "spaspm_formula"),
          function(spaspm_object) spaspm_object@raw_formula
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setGeneric(name = "raw_formula<-",
           def = function(object, value) standardGeneric("raw_formula<-")
)

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setMethod("raw_formula<-",
          signature("object" = "spaspm_formula"),
          function(object, value){
            object@raw_formula <- value
            validObject(object)
            return(object)
          }
)

# Translated_formula ------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setGeneric(name = "translated_formula",
           def = function(spaspm_object) standardGeneric("translated_formula")
)

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setMethod("translated_formula",
          signature("spaspm_object" = "spaspm_formula"),
          function(spaspm_object) spaspm_object@translated_formula
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setGeneric(name = "translated_formula<-",
           def = function(object, value) standardGeneric("translated_formula<-")
)

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setMethod("translated_formula<-",
          signature("object" = "spaspm_formula"),
          function(object, value){
            object@translated_formula <- value
            validObject(object)
            return(object)
          }
)

# Dataset -----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setGeneric(name = "dataset",
           def = function(spaspm_object) standardGeneric("dataset")
)

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setMethod("dataset",
          signature("spaspm_object" = "spaspm_formula"),
          function(spaspm_object) spaspm_object@dataset
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setGeneric(name = "dataset<-",
           def = function(object, value) standardGeneric("dataset<-")
)

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setMethod("dataset<-",
          signature("object" = "spaspm_formula"),
          function(object, value){
            object@dataset <- value
            validObject(object)
            return(object)
          }
)

# Vars --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setGeneric(name = "formula_vars",
           def = function(spaspm_object) standardGeneric("formula_vars")
)

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setMethod("formula_vars",
          signature("spaspm_object" = "spaspm_formula"),
          function(spaspm_object) spaspm_object@vars
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setGeneric(name = "formula_vars<-",
           def = function(object, value) standardGeneric("formula_vars<-")
)

#' @describeIn accessors-methods-spaspm_formula TODO
#' @export
setMethod("formula_vars<-",
          signature("object" = "spaspm_formula"),
          function(object, value){
            object@vars <- value
            validObject(object)
            return(object)
          }
)
