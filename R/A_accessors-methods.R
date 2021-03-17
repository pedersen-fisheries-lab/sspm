#' Accessing OR replacing `sspm` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [sspm][sspm-class] and others derivative classes
#' (`sspm_discrete`, etc...).
#'
#' @param sspm_object **\[sspm OR adjacent\]** An object of class
#'     [sspm][sspm-class] or others derivative classes.
#' @inheritParams base::Extract
#'
#' @aliases spm_
#' @rdname sspm-accessors-methods

# Name --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "spm_name",
           def = function(sspm_object) standardGeneric("spm_name")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_name", signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_name<-",
           def = function(object, value) standardGeneric("spm_name<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_name<-",
          signature("object" = "sspm"),
          function(object, value){
            object@name <- value
            validObject(object)
            return(object)
          }
)

# Datasets ----------------------------------------------------------------
# Accesors ----------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_datasets",
           def = function(sspm_object) standardGeneric("spm_datasets")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_datasets", signature("sspm_object" = "sspm"),
          function(sspm_object) {
            datasets <- sspm_object@datasets
            names(datasets) <- sapply(datasets, spm_name)
            return(datasets)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_datasets", signature("sspm_object" = "sspm_data"),
          function(sspm_object) {
            cli::cli_alert_danger("Use `spm_data` to access the data of a dataset object")
          }
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_datasets<-",
           def = function(object, value) standardGeneric("spm_datasets<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_datasets<-",
          signature("object" = "sspm"),
          function(object, value){
            object@datasets <- value
            validObject(object)
            return(object)
          }
)

# ID ----------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_unique_ID",
           def = function(sspm_object) standardGeneric("spm_unique_ID")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_unique_ID",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@data@uniqueID
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_unique_ID",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) {
            if(length(spm_mapped_datasets(sspm_object)) > 0){
              names <- lapply(spm_datasets(sspm_object),
                              spm_unique_ID)
            } else {
              names <- sspm_object@data@uniqueID
            }
            return(names)
          }
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_unique_ID<-",
           def = function(object, value) standardGeneric("spm_unique_ID<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_unique_ID<-",
          signature("object" = "sspm"),
          function(object, value){
            object@data@uniqueID <- value
            validObject(object)
            return(object)
          }
)

# Coords ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_coords_col",
           def = function(sspm_object) standardGeneric("spm_coords_col")
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_coords_col<-",
           def = function(object, value) standardGeneric("spm_coords_col<-")
)

# Time col ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_time_column",
           def = function(sspm_object) standardGeneric("spm_time_column")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_time_column",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@data@time_column
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_time_column",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) {
            if(length(spm_mapped_datasets(sspm_object)) > 0){
              cols <- lapply(spm_datasets(sspm_object),
                             spm_time_column)
            } else {
              cols <- sspm_object@data@time_column
            }
            return(cols)
          }
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_time_column<-",
           def = function(object, value) standardGeneric("spm_time_column<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_time_column<-",
          signature("object" = "sspm"),
          function(object, value){
            object@data@time_column <- value
            validObject(object)
            return(object)
          }
)

# Boundaries --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_boundaries",
           def = function(sspm_object) standardGeneric("spm_boundaries")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_boundaries", signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@boundaries
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_boundaries<-",
           def = function(object, value) standardGeneric("spm_boundaries<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@boundaries <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "sspm"),
          function(object, value){
            object@boundaries <- value
            validObject(object)
            return(object)
          }
)

# DISCRETE BEYOND THIS POINT ----------------------------------------------

# Discretization method ---------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_discret_method",
           def = function(sspm_object) standardGeneric("spm_discret_method")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_discret_method",
          signature("sspm_object" = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_discret_method",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) sspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_discret_method<-",
           def = function(object, value) standardGeneric("spm_discret_method<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_discret_method<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@method <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_discret_method<-",
          signature("object" = "sspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# Patches -----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_patches",
           def = function(sspm_object) standardGeneric("spm_patches")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_patches",
          signature("sspm_object" = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_patches",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) sspm_object@patches
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_patches<-",
           def = function(object, value) standardGeneric("spm_patches<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_patches<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@patches <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_patches<-",
          signature("object" = "sspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# Points ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_points",
           def = function(sspm_object) standardGeneric("spm_points")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_points",
          signature("sspm_object" = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_points",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) sspm_object@points
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_points<-",
           def = function(object, value) standardGeneric("spm_points<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_points<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@points <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_points<-",
          signature("object" = "sspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# -------------------------------------------------------------------------
# Mapped formulas ---------------------------------------------------------

# Accesors ----------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_mapped_formulas",
           def = function(sspm_object) standardGeneric("spm_mapped_formulas")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_mapped_formulas",
          signature("sspm_object" = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_mapped_formulas",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) sspm_object@mapped_formulas
)

# Replacers ---------------------------------------------------------------

#' @describeIn sspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_mapped_formulas<-",
           def = function(object, value) standardGeneric("spm_mapped_formulas<-")
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_mapped_formulas<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@mapped_formulas <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn sspm-accessors-methods TODO
#' @export
setMethod("spm_mapped_formulas<-",
          signature("object" = "sspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# TODO dim should get dims of data and sf if discrete
# setMethod("dim",
#           "sspm", function(x) length(x@snpid))
