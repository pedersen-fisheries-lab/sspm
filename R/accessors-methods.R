#' Accessing OR replacing `spaspm` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [spaspm][spaspm-class] and others derivative classes
#' (`spaspm_discrete`, etc...).
#'
#' @param spaspm_object **\[spaspm OR adjacent\]** An object of class
#'     [spaspm][spaspm-class] or others derivative classes.
#'
#' @rdname spaspm-accessors-methods

# Name --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @export
setGeneric(name = "spm_name",
           def = function(spaspm_object) standardGeneric("spm_name")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_name<-",
           def = function(object, value) standardGeneric("spm_name<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_name<-",
          signature("object" = "spaspm"),
          function(object, value){
            object@name <- value
            validObject(object)
            return(object)
          }
)

# Base dataset ------------------------------------------------------------
# Accesors ----------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_base_dataset",
           def = function(spaspm_object) standardGeneric("spm_base_dataset")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_base_dataset", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_base_dataset", signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@data
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_base_dataset<-",
           def = function(object, value) standardGeneric("spm_base_dataset<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_base_dataset<-",
          signature("object" = "spaspm"),
          function(object, value){
            object@data <- value
            validObject(object)
            return(object)
          }
)

# Datasets ----------------------------------------------------------------
# No replacers here as we aggregate different datasets 'types'

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_datasets",
           def = function(spaspm_object) standardGeneric("spm_datasets")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_datasets", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_datasets", signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object){
            if(length(spm_mapped_datasets(spaspm_object)) > 0){
              # A bit ugly but required to format list of different length
              # without using purrr
              datasets <- unlist(list(list(Main_dataset = spm_base_dataset(spaspm_object)),
                                      Mapped_datasets = spm_mapped_datasets(spaspm_object)),
                                 recursive = FALSE)
              # Add names
              names(datasets) <- c(spm_name(spm_base_dataset(spaspm_object)),
                                   sapply(spm_mapped_datasets(spaspm_object), spm_name))
            } else {
              datasets <- spm_base_dataset(spaspm_object)
            }
            return(datasets)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_datasets", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) {
            cli::cli_alert_danger("Use `spm_data` to access the data of a dataset object")
          }
)

# ID ----------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_unique_ID",
           def = function(spaspm_object) standardGeneric("spm_unique_ID")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data@uniqueID
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@uniqueID
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) {
            if(length(spm_mapped_datasets(spaspm_object)) > 0){
              names <- lapply(spm_datasets(spaspm_object),
                              spm_unique_ID)
            } else {
              names <- spaspm_object@data@uniqueID
            }
            return(names)
          }
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_unique_ID<-",
           def = function(object, value) standardGeneric("spm_unique_ID<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_unique_ID<-",
          signature("object" = "spaspm"),
          function(object, value){
            object@unique_ID <- value
            validObject(object)
            return(object)
          }
)

# Coords ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_coords_col",
           def = function(spaspm_object) standardGeneric("spm_coords_col")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_coords_col",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data@coords
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_coords_col",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@coords
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_coords_col",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) {
            if(length(spm_mapped_datasets(spaspm_object)) > 0){
              coords <- lapply(spm_datasets(spaspm_object),
                               spm_coords_col)
            } else {
              coords <- spaspm_object@data@coords
            }
            return(coords)
          }
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_coords_col<-",
           def = function(object, value) standardGeneric("spm_coords_col<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_coords_col<-",
          signature("object" = "spaspm"),
          function(object, value){
            object@coords <- value
            validObject(object)
            return(object)
          }
)

# DISCRETE BEYOND THIS POINT ----------------------------------------------

# Boundaries --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_boundaries",
           def = function(spaspm_object) standardGeneric("spm_boundaries")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_boundaries", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@boundaries
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_boundaries<-",
           def = function(object, value) standardGeneric("spm_boundaries<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "spaspm_discrete"),
          function(object, value){
            object@boundaries <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "spaspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# Discretization method ---------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_discret_method",
           def = function(spaspm_object) standardGeneric("spm_discret_method")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_discret_method<-",
           def = function(object, value) standardGeneric("spm_discret_method<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_discret_method<-",
          signature("object" = "spaspm_discrete"),
          function(object, value){
            object@method <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "spaspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# Patches -----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_patches",
           def = function(spaspm_object) standardGeneric("spm_patches")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_patches",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_patches",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@patches
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_patches<-",
           def = function(object, value) standardGeneric("spm_patches<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_patches<-",
          signature("object" = "spaspm_discrete"),
          function(object, value){
            object@patches <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_patches<-",
          signature("object" = "spaspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# Points ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_points",
           def = function(spaspm_object) standardGeneric("spm_points")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_points",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_points",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@points
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_points<-",
           def = function(object, value) standardGeneric("spm_points<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_points<-",
          signature("object" = "spaspm_discrete"),
          function(object, value){
            object@points <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_points<-",
          signature("object" = "spaspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# Mapped datasets ---------------------------------------------------------
# Accesors ----------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_mapped_datasets",
           def = function(spaspm_object) standardGeneric("spm_mapped_datasets")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_mapped_datasets",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_mapped_datasets",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@mapped_datasets
)

# Replacers ---------------------------------------------------------------

#' @describeIn spaspm-accessors-methods TODO
#' @export
setGeneric(name = "spm_mapped_datasets<-",
           def = function(object, value) standardGeneric("spm_mapped_datasets<-")
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_mapped_datasets<-",
          signature("object" = "spaspm_discrete"),
          function(object, value){
            object@mapped_datasets <- value
            validObject(object)
            return(object)
          }
)

#' @describeIn spaspm-accessors-methods TODO
#' @export
setMethod("spm_mapped_datasets<-",
          signature("object" = "spaspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# -------------------------------------------------------------------------

# TODO dim should get dims of data and sf if discrete
# setMethod("dim",
#           "spaspm", function(x) length(x@snpid))
