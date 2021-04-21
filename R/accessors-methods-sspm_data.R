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
#' @rdname accessors-methods-sspm_data
setMethod("spm_data", signature("sspm_object" = "sspm"),
          function(sspm_object) {
            cli::cli_alert_danger("Use `spm_datasets` or `spm_base_dataset` to access the datasets of a sspm object")
          }
)

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_data", signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@data
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_data<-",
           def = function(object, value) standardGeneric("spm_data<-")
)

#' @rdname accessors-methods-sspm_data
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

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_name", signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_name<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@name <- value
            validObject(object)
            return(object)
          }
)

# Unique ID ---------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_unique_ID",
           def = function(sspm_object) standardGeneric("spm_unique_ID")
)

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_unique_ID",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@uniqueID
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_unique_ID<-",
           def = function(object, value) standardGeneric("spm_unique_ID<-")
)

#' @rdname accessors-methods-sspm_data
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

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_coords_col",
           def = function(sspm_object) standardGeneric("spm_coords_col")
)

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_coords_col",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@coords
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_coords_col<-",
           def = function(object, value) standardGeneric("spm_coords_col<-")
)

#' @rdname accessors-methods-sspm_data
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

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_time_column",
           def = function(sspm_object) standardGeneric("spm_time_column")
)

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_time_column",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@time_column
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_time_column<-",
           def = function(object, value) standardGeneric("spm_time_column<-")
)

#' @rdname accessors-methods-sspm_data
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

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_formulas",
           def = function(sspm_object) standardGeneric("spm_formulas")
)

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_formulas",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@formulas
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_formulas<-",
           def = function(object, value) standardGeneric("spm_formulas<-")
)

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_formulas<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@formulas <- value
            validObject(object)
            return(object)
          }
)

# Type --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_type",
           def = function(sspm_object) standardGeneric("spm_type")
)

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_type",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@type
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setGeneric(name = "spm_type<-",
           def = function(object, value) standardGeneric("spm_type<-")
)

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_type<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@type <- value
            validObject(object)
            return(object)
          }
)

# Smoothed data -----------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_smoothed_data",
          signature("sspm_object" = "sspm_data"),
          function(sspm_object) sspm_object@smoothed_data
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_data
#' @export
setMethod("spm_smoothed_data<-",
          signature("object" = "sspm_data"),
          function(object, value){
            object@smoothed_data <- value
            validObject(object)
            return(object)
          }
)
