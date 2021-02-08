#' Accessing OR replacing `spaspm` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [spaspm][spaspm-class] and others derivative classes
#' (`spaspm_discrete`, etc...).
#'
#' @param spaspm_object **\[spaspm OR adjacent\]** An object of class
#'     [spaspm][spaspm-class] or others derivative classes.
#'
#' @rdname accessors-methods
#' @export
setGeneric(name = "spm_name",
           def = function(spaspm_object) standardGeneric("spm_name")
)

# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@name
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@name
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "discretization_method"),
          function(spaspm_object) spaspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_name<-",
           def = function(object, value) standardGeneric("spm_name<-")
)

# Base dataset ------------------------------------------------------------
# Accesors ----------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_base_dataset",
           def = function(spaspm_object) standardGeneric("spm_base_dataset")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_base_dataset", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_base_dataset", signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@data
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_base_dataset<-",
           def = function(object, value) standardGeneric("spm_base_dataset<-")
)

# Datasets ----------------------------------------------------------------
# No replacers here as we aggregate different datasets 'types'

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_datasets",
           def = function(spaspm_object) standardGeneric("spm_datasets")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_datasets", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @describeIn accessors-methods TODO
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

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_datasets", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) {
            cli::cli_alert_danger("Use `spm_data` to access the data of a dataset object")
          }
)

# Data --------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_data",
           def = function(spaspm_object) standardGeneric("spm_data")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) {
            cli::cli_alert_danger("Use `spm_datasets` to access the datasets of a spaspm object")
          }
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@data
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_data<-",
           def = function(object, value) standardGeneric("spm_data<-")
)

# ID ----------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_unique_ID",
           def = function(spaspm_object) standardGeneric("spm_unique_ID")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data@uniqueID
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@uniqueID
)

#' @describeIn accessors-methods TODO
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

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_unique_ID<-",
           def = function(object, value) standardGeneric("spm_unique_ID<-")
)

# Coords ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_coords_col",
           def = function(spaspm_object) standardGeneric("spm_coords_col")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_coords_col",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data@coords
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_coords_col",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@coords
)

#' @describeIn accessors-methods TODO
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

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_coords_col<-",
           def = function(object, value) standardGeneric("spm_coords_col<-")
)

# Boundaries --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_boundaries",
           def = function(spaspm_object) standardGeneric("spm_boundaries")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_boundaries", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@boundaries
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_boundaries<-",
           def = function(object, value) standardGeneric("spm_boundaries<-")
)

# Discretization method ---------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_discret_method",
           def = function(spaspm_object) standardGeneric("spm_discret_method")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_discret_method<-",
           def = function(object, value) standardGeneric("spm_discret_method<-")
)

# For method --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "method_func",
           def = function(spaspm_object) standardGeneric("method_func")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("method_func",
          signature("spaspm_object" = "discretization_method"),
          function(spaspm_object) spaspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "method_func<-",
           def = function(object, value) standardGeneric("method_func<-")
)

# Patches -----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_patches",
           def = function(spaspm_object) standardGeneric("spm_patches")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_patches",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_patches",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@patches
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_patches<-",
           def = function(object, value) standardGeneric("spm_patches<-")
)

# Points ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_points",
           def = function(spaspm_object) standardGeneric("spm_points")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_points",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_points",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@points
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_points<-",
           def = function(object, value) standardGeneric("spm_points<-")
)

# Mapped datasets ---------------------------------------------------------
# Accesors ----------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_mapped_datasets",
           def = function(spaspm_object) standardGeneric("spm_mapped_datasets")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_mapped_datasets",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_mapped_datasets",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@mapped_datasets
)

# Replacers ---------------------------------------------------------------

#' @describeIn accessors-methods TODO
#' @export
setGeneric(name = "spm_mapped_datasets<-",
           def = function(object, value) standardGeneric("spm_mapped_datasets<-")
)

#' @describeIn accessors-methods TODO
#' @export
setMethod("spm_mapped_datasets<-",
          signature("object" = "spaspm_discrete"),
          function(object, value){
            object@mapped_datasets <- value
            return(object)
          }
)

#' @describeIn accessors-methods TODO
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
