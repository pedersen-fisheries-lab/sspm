#' Accessing OR replacing `spaspm_data` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [spaspm_data][spaspm_data-class].
#'
#' @param spaspm_object **\[spaspm_data\]** An object of class
#'     [spaspm_data][spaspm_data-class].
#' @inheritParams base::Extract
#'
#' @rdname spaspm_data-accessors-methods

# Data --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "spm_data",
           def = function(spaspm_object) standardGeneric("spm_data")
)

#' @export
#' @describeIn spaspm_data-accessors-methods TODO
setMethod("spm_data", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) {
            cli::cli_alert_danger("Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")
          }
)

#' @describeIn spaspm_data-accessors-methods TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@data
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm_data-accessors-methods TODO
#' @export
setGeneric(name = "spm_data<-",
           def = function(object, value) standardGeneric("spm_data<-")
)

#' @describeIn spaspm_data-accessors-methods TODO
#' @export
setMethod("spm_data<-",
          signature("object" = "spaspm_data"),
          function(object, value){
            object@data <- value
            validObject(object)
            return(object)
          }
)

# Name --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm_data-accessors-methods TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm_data-accessors-methods TODO
#' @export
setMethod("spm_name<-",
          signature("object" = "spaspm_data"),
          function(object, value){
            object@name <- value
            validObject(object)
            return(object)
          }
)

# Rep ---------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "spm_rep",
           def = function(spaspm_object) standardGeneric("spm_rep")
)

#' @export
#' @describeIn spaspm_data-accessors-methods TODO
setMethod("spm_rep", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) {
            function(spaspm_object) spaspm_object@representation
          }
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm_data-accessors-methods TODO
#' @export
setGeneric(name = "spm_rep<-",
           def = function(object, value) standardGeneric("spm_rep<-")
)

#' @describeIn spaspm_data-accessors-methods TODO
#' @export
setMethod("spm_rep<-",
          signature("object" = "spaspm_data"),
          function(object, value){
            object@representation <- value
            validObject(object)
            return(object)
          }
)
