#' Accessing `spaspm` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [spaspm][spaspm-class] and others derivative classes
#' (`spaspm_discrete`, etc...).
#'
#' @param spaspm_object **\[spaspm OR adjacent\]** An object of class
#'     [spaspm][spaspm-class] or others derivative classes.
#'
#' @rdname accessors
#' @export
setGeneric(name = "spm_name",
           def = function(spaspm_object) standardGeneric("spm_name")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@name
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@name
)


# Base dataset ------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_base_dataset",
           def = function(spaspm_object) standardGeneric("spm_base_dataset")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_base_dataset", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_base_dataset", signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@data
)

# Datasets ----------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_datasets",
           def = function(spaspm_object) standardGeneric("spm_datasets")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_datasets", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_datasets", signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object){
            if(length(spaspm_object@mapped_datasets) > 0){
              datasets <- unlist(list(list(Main_dataset = spaspm_object@data),
                                      Mapped_datasets = spaspm_object@mapped_datasets),
                                 recursive = FALSE)
              names(datasets) <- c(spaspm_object@data@name,
                                   sapply(spm_name, spaspm_object@mapped_datasets))
            } else {
              datasets <- spaspm_object@data
            }
            return(datasets)
          }
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_datasets", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) {
            cli::cli_alert_danger("Use `spm_data` to access the data of a dataset object")
          }
)

# Data --------------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_data",
           def = function(spaspm_object) standardGeneric("spm_data")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) {
            cli::cli_alert_danger("Use `spm_datasets` to access the datasets of a spaspm object")
          }
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@data
)

# ID ----------------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_unique_ID",
           def = function(spaspm_object) standardGeneric("spm_unique_ID")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data@uniqueID
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@uniqueID
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_unique_ID",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) {
            # TODO adjust this for list
            spaspm_object@data@uniqueID
          }
)

# Coords ------------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_coords_col",
           def = function(spaspm_object) standardGeneric("spm_coords_col")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_coords_col",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data@coords
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_coords_col",
          signature("spaspm_object" = "spaspm_data"),
          function(spaspm_object) spaspm_object@coords
)

# Boundaries --------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_boundaries",
           def = function(spaspm_object) standardGeneric("spm_boundaries")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_boundaries", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@boundaries
)

# Discretization method ---------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_discret_method",
           def = function(spaspm_object) standardGeneric("spm_discret_method")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@method
)

# Patches -----------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_patches",
           def = function(spaspm_object) standardGeneric("spm_patches")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_patches",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_patches",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@patches
)

# Points ------------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_points",
           def = function(spaspm_object) standardGeneric("spm_points")
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_points",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_points",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@points
)

# -------------------------------------------------------------------------

# TODO dim should get dims of data and sf if discrete
# setMethod("dim",
#           "spaspm", function(x) length(x@snpid))
