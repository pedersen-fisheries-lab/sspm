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

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_name", signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@name
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_name<-",
           def = function(object, value) standardGeneric("spm_name<-")
)

#' @rdname sspm-accessors-methods
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

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_datasets",
           def = function(sspm_object, type = NULL) standardGeneric("spm_datasets")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_datasets", signature("sspm_object" = "sspm"),
          function(sspm_object, type) {
            datasets <- sspm_object@datasets

            if(!is.null(type)){

              checkmate::assert_choice(type, c("biomass", "catch", "predictor"))

              types <- sapply(datasets, spm_type)
              idx <- which(types == type)
              datasets <- datasets[[idx]]

            }

            names(datasets) <- sapply(datasets, spm_name)
            return(datasets)
          }
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_datasets", signature("sspm_object" = "sspm_data"),
          function(sspm_object) {
            cli::cli_alert_danger("Use `spm_data` to access the data of a dataset object")
          }
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_datasets<-",
           def = function(object, value) standardGeneric("spm_datasets<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_datasets<-",
          signature("object" = "sspm"),
          function(object, value){
            object@datasets <- value
            validObject(object)
            return(object)
          }
)

# Boundaries --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_boundaries",
           def = function(sspm_object) standardGeneric("spm_boundaries")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_boundaries", signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@boundaries
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_boundaries<-",
           def = function(object, value) standardGeneric("spm_boundaries<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@boundaries <- value
            validObject(object)
            return(object)
          }
)

#' @rdname sspm-accessors-methods
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

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_discret_method",
           def = function(sspm_object) standardGeneric("spm_discret_method")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_discret_method",
          signature("sspm_object" = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_discret_method",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) sspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_discret_method<-",
           def = function(object, value) standardGeneric("spm_discret_method<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_discret_method<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@method <- value
            validObject(object)
            return(object)
          }
)

#' @rdname sspm-accessors-methods
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

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_patches",
           def = function(sspm_object) standardGeneric("spm_patches")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_patches",
          signature("sspm_object" = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_patches",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) sspm_object@patches
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_patches<-",
           def = function(object, value) standardGeneric("spm_patches<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_patches<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@patches <- value
            validObject(object)
            return(object)
          }
)

#' @rdname sspm-accessors-methods
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

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_points",
           def = function(sspm_object) standardGeneric("spm_points")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_points",
          signature("sspm_object" = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_points",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) sspm_object@points
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_points<-",
           def = function(object, value) standardGeneric("spm_points<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_points<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@points <- value
            validObject(object)
            return(object)
          }
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_points<-",
          signature("object" = "sspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# TODO dim should get dims of data and sf if discrete
# setMethod("dim",
#           "sspm", function(x) length(x@snpid))

# Smoothed data -----------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_smoothed_data",
           def = function(sspm_object) standardGeneric("spm_smoothed_data")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_smoothed_data",
          signature("sspm_object" = "sspm"),
          function(sspm_object){
            message_not_discrete(sspm_object)
          }
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_smoothed_data",
          signature("sspm_object" = "sspm_discrete"),
          function(sspm_object) sspm_object@smoothed_data
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_smoothed_data<-",
           def = function(object, value) standardGeneric("spm_smoothed_data<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_smoothed_data<-",
          signature("object" = "sspm_discrete"),
          function(object, value){
            object@smoothed_data <- value
            validObject(object)
            return(object)
          }
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_smoothed_data<-",
          signature("object" = "sspm"),
          function(object, value){
            message_not_discrete(object)
            return(object)
          }
)

# Formulas ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_formulas",
           def = function(sspm_object) standardGeneric("spm_formulas")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_formulas",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@formulas
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_formulas<-",
           def = function(object, value) standardGeneric("spm_formulas<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_formulas<-",
          signature("object" = "sspm"),
          function(object, value){
            object@formulas <- value
            validObject(object)
            return(object)
          }
)
