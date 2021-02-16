#' Accessing OR replacing `spaspm_smooth` object elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [spaspm_smooth][spaspm_smooth-class].
#'
#' @param spaspm_object **\[spaspm_smooth\]** An object of class
#'    [spaspm_smooth][spaspm_smooth-class].
#' @inheritParams base::Extract
#'
#' @rdname spaspm_smooth-accessors-methods

# Rep ---------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setMethod("spm_rep", signature("spaspm_object" = "spaspm_smooth"),
          function(spaspm_object) spaspm_object@representation
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setMethod("spm_rep<-",
          signature("object" = "spaspm_smooth"),
          function(object, value){
            object@representation <- value
            validObject(object)
            return(object)
          }
)

# Smooth
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setGeneric(name = "spm_smooth",
           def = function(spaspm_object) standardGeneric("spm_smooth")
)

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setMethod("spm_smooth", signature("spaspm_object" = "spaspm_smooth"),
          function(spaspm_object) spaspm_object@smooth
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setGeneric(name = "spm_smooth<-",
           def = function(object, value) standardGeneric("spm_smooth<-")
)

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setMethod("spm_smooth<-",
          signature("object" = "spaspm_smooth"),
          function(object, value){
            object@smooth <- value
            validObject(object)
            return(object)
          }
)

# Dataset name ------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setGeneric(name = "spm_dataset_name",
           def = function(spaspm_object) standardGeneric("spm_dataset_name")
)

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setMethod("spm_dataset_name", signature("spaspm_object" = "spaspm_smooth"),
          function(spaspm_object) spaspm_object@dataset_name
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setGeneric(name = "spm_dataset_name<-",
           def = function(object, value) standardGeneric("spm_dataset_name<-")
)

#' @describeIn spaspm_smooth-accessors-methods TODO
#' @export
setMethod("spm_dataset_name<-",
          signature("object" = "spaspm_smooth"),
          function(object, value){
            object@dataset_name <- value
            validObject(object)
            return(object)
          }
)
