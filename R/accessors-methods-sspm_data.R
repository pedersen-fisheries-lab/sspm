#' Accessing OR replacing `sspm_data` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [sspm_data][sspm_data-class].
#'
#' @param sspm_object **\[sspm_data\]** An object of class
#'     [sspm_data][sspm_data-class].
#' @inheritParams base::Extract
#'
#' @rdname accessors-methods-sspm_data

# Data --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "spm_data",
           def = function(sspm_object) standardGeneric("spm_data")
)

#' @export
#' @describeIn accessors-methods-sspm_data TODO
setMethod("spm_data", signature("sspm_object" = "sspm"),
          function(sspm_object) {
            cli::cli_alert_danger("Use `spm_datasets` or `spm_base_dataset` to access the datasets of a sspm object")
          }
)

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_data", signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@data
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setGeneric(name = "spm_data<-",
           def = function(object, value) standardGeneric("spm_data<-")
)

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_data<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@data <- value
            validObject(object)
            return(object)
          }
)

# Name --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_name", signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_name<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@name <- value
            validObject(object)
            return(object)
          }
)

# Rep ---------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setGeneric(name = "spm_rep",
           def = function(sspm_object) standardGeneric("spm_rep")
)

#' @export
#' @describeIn accessors-methods-sspm_data TODO
setMethod("spm_rep", signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@representation
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setGeneric(name = "spm_rep<-",
           def = function(object, value) standardGeneric("spm_rep<-")
)

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_rep<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@representation <- value
            validObject(object)
            return(object)
          }
)

# Unique ID ---------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_unique_ID",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@uniqueID
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_unique_ID<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@uniqueID <- value
            validObject(object)
            return(object)
          }
)

# Coords ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_coords_col",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@coords
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_coords_col<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@coords <- value
            validObject(object)
            return(object)
          }
)

# Time col ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_time_column",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@time_column
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_time_column<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@time_column <- value
            validObject(object)
            return(object)
          }
)

# Formulas ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_mapped_formulas",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@mapped_formulas
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-sspm_data TODO
#' @export
setMethod("spm_mapped_formulas<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@mapped_formulas <- value
            validObject(object)
            return(object)
          }
)
