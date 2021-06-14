#' Accessing OR replacing `sspm` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [sspm][sspm-class] and others derivative classes
#' (`sspm_discrete`, etc...).
#'
#' @param sspm_object **\[sspm OR adjacent\]** An object of class
#'     [sspm][sspm-class] or others derivative classes.
#' @inheritParams base::Extract
#' @inheritParams map_dataset
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
          signature("object" = "sspm"),
          function(object, value){
            object@smoothed_data <- value
            validObject(object)
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

# #' @rdname sspm-accessors-methods
# #' @export
# setMethod("spm_formulas",
#           signature("sspm_object" = "sspm"),
#           function(sspm_object) sspm_object@formulas
# )

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_formulas<-",
           def = function(object, value) standardGeneric("spm_formulas<-")
)

# #' @rdname sspm-accessors-methods
# #' @export
# setMethod("spm_formulas<-",
#           signature("object" = "sspm"),
#           function(object, value){
#             object@formulas <- value
#             validObject(object)
#             return(object)
#           }
# )

# Time col ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_time_column",
           def = function(sspm_object) standardGeneric("spm_time_column")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_time_column",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@time_column
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "spm_time_column<-",
           def = function(object, value) standardGeneric("spm_time_column<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("spm_time_column<-",
          signature("object" = "sspm"),
          function(object, value){
            object@time_column <- value
            validObject(object)
            return(object)
          }
)

# Is split ----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "is_split",
           def = function(sspm_object) standardGeneric("is_split")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("is_split",
          signature("sspm_object" = "sspm"),
          function(sspm_object) sspm_object@is_split
)

# Replacers ---------------------------------------------------------------

#' @rdname sspm-accessors-methods
#' @export
setGeneric(name = "is_split<-",
           def = function(object, value) standardGeneric("is_split<-")
)

#' @rdname sspm-accessors-methods
#' @export
setMethod("is_split<-",
          signature("object" = "sspm"),
          function(object, value){
            object@is_split <- value
            validObject(object)
            return(object)
          }
)
