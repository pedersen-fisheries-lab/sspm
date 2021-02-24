#' Accessing OR replacing `spaspm_data` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [spaspm_data][spaspm_data-class].
#'
#' @param spaspm_object **\[spaspm_data\]** An object of class
#'     [spaspm_data][spaspm_data-class].
#' @inheritParams base::Extract
#'
#' @rdname accessors-methods-spaspm_data

# Data --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "spm_data",
           def = function(spaspm_object) standardGeneric("spm_data")
)

#' @export
#' @describeIn accessors-methods-spaspm_data TODO
setMethod("spm_data", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) {
            cli::cli_alert_danger("Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")
          }
)

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@data
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setGeneric(name = "spm_data<-",
           def = function(object, value) standardGeneric("spm_data<-")
)

#' @describeIn accessors-methods-spaspm_data TODO
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

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
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

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setGeneric(name = "spm_rep",
           def = function(spaspm_object) standardGeneric("spm_rep")
)

#' @export
#' @describeIn accessors-methods-spaspm_data TODO
setMethod("spm_rep", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@representation
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setGeneric(name = "spm_rep<-",
           def = function(object, value) standardGeneric("spm_rep<-")
)

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_rep<-",
          signature("object" = "spaspm_data"),
          function(object, value){
            object@representation <- value
            validObject(object)
            return(object)
          }
)

# Unique ID ---------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@uniqueID
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_unique_ID<-",
          signature("object" = "spaspm_data"),
          function(object, value){
            object@unique_ID <- value
            validObject(object)
            return(object)
          }
)

# Coords ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_coords_col",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@coords
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_coords_col<-",
          signature("object" = "spaspm_data"),
          function(object, value){
            object@coords <- value
            validObject(object)
            return(object)
          }
)

# Time col ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_time_col",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@time_col
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods-spaspm_data TODO
#' @export
setMethod("spm_time_col<-",
          signature("object" = "spaspm_data"),
          function(object, value){
            object@time_col <- value
            validObject(object)
            return(object)
          }
)
