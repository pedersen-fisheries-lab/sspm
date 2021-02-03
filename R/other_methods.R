# Package Generics --------------------------------------------------------

# Exported ----------------------------------------------------------------

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
setGeneric(name = "spm_boundaries",
           def = function(spaspm_object) standardGeneric("spm_boundaries")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_data",
           def = function(spaspm_object) standardGeneric("spm_data")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_name",
           def = function(spaspm_object) standardGeneric("spm_name")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_discret_method",
           def = function(spaspm_object) standardGeneric("spm_discret_method")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_patches",
           def = function(spaspm_object) standardGeneric("spm_patches")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_points",
           def = function(spaspm_object) standardGeneric("spm_points")
)

#' @describeIn accessors TODO
#' @export
setGeneric(name = "spm_id",
           def = function(spaspm_object) standardGeneric("spm_id")
)

# Methods for package generics --------------------------------------------

# Exported ----------------------------------------------------------------

#' @describeIn accessors TODO
#' @export
setMethod("spm_name", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@name
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@data
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_data", signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) list(data = spaspm_object@data,
                                       data_spatial = spaspm_object@data_spatial)
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_boundaries", signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@boundaries
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
setMethod("spm_patches",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object){
            message_not_discrete(spaspm_object)
          }
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
setMethod("spm_id",
          signature("spaspm_object" = "spaspm"),
          function(spaspm_object) spaspm_object@ID
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_discret_method",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@method
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_patches",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@patches
)

#' @describeIn accessors TODO
#' @export
setMethod("spm_points",
          signature("spaspm_object" = "spaspm_discrete"),
          function(spaspm_object) spaspm_object@points
)

# Methods for global generics ---------------------------------------------

# Show method for spaspm object
setMethod("show",
          "spaspm",
          function(object) {
            cli::cli_h2(cli::col_blue(cli::style_bold("SPASPM object '", object@name, "'")))
            show(object@data)
            cat_boundaries(object)
            cat("\n")
          }
)

setMethod("show",
          "spaspm_discrete",
          function(object) {
            cli::cli_h2(cli::col_blue(cli::style_bold("SPASPM object '", object@name, "' ",
                                                      cli::col_green("(DISCRETIZED)"))))
            show(object@data)
            cat_boundaries(object)
            cat_discretization_info(object)
            if (length(object@mapped_datasets) > 1){
              cat_mapped_datasets(object)
            }
            cat("\n")
          }
)

setMethod("show",
          "discretization_method",
          function(object) {
            cli::cli_h3(cli::col_cyan("Discretization method"))
            cli::cat_bullet(" Name             : '", object@name, "'")
            # TODO manage to print function name
            # cli::cat_bullet(" Function         :", object@method)
          }
)

setMethod("show",
          "spaspm_data",
          function(object) {
            cli::cli_h3(cli::col_cyan("SPASPM Dataset '", object@name, "' "))
            cli::cat_bullet(" Data matrix      : ", object@representation, " with ",
                            dim(object@data)[1], " feature(s) and ",
                            dim(object@data)[2], " variable(s)")
            cli::cat_bullet(" Data unique ID   : ", cli::col_green(object@uniqueID))
            if(!is.null(object@coords)){
              cli::cat_bullet(" Coordinates cols : ",
                              paste(cli::col_green(object@coords), collapse = ", "))
            }
          }
)

# TODO dim should get dims of data and sf if discrete
# setMethod("dim",
#           "spaspm", function(x) length(x@snpid))
