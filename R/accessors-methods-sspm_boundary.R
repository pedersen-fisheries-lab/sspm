#' Accessing OR replacing `sspm_boundary` model elements
#'
#' All methods described here allow to access the elements of contained in
#' objects of class [sspm_boundary][sspm_boundary-class].
#'
#' @param sspm_object **\[sspm_boundary\]** An object of class
#'     [sspm_boundary][sspm_boundary-class].
#'
#' @inheritParams base::Extract
#'

# Boundaries --------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundaries", signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@boundaries
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundaries<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@boundaries <- value
            validObject(object)
            return(object)
          }
)

# DISCRETE BEYOND THIS POINT ----------------------------------------------

# Discretization method ---------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_discret_method",
           def = function(sspm_object) standardGeneric("spm_discret_method")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_discret_method",
          signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@method
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_discret_method<-",
           def = function(object, value) standardGeneric("spm_discret_method<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_discret_method<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@method <- value
            validObject(object)
            return(object)
          }
)

# Patches -----------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_patches",
           def = function(sspm_object) standardGeneric("spm_patches")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_patches",
          signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@patches
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_patches<-",
           def = function(object, value) standardGeneric("spm_patches<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_patches<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@patches <- value
            validObject(object)
            return(object)
          }
)

# Points ------------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_points",
           def = function(sspm_object) standardGeneric("spm_points")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_points",
          signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@points
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_points<-",
           def = function(object, value) standardGeneric("spm_points<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_points<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@points <- value
            validObject(object)
            return(object)
          }
)

# Boundary col ------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_boundary_colum",
           def = function(sspm_object) standardGeneric("spm_boundary_colum")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundary_colum",
          signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@boundary_column
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_boundary_colum<-",
           def = function(object, value) standardGeneric("spm_boundary_colum<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_boundary_colum<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@boundary_column <- value
            validObject(object)
            return(object)
          }
)

# Surface col -------------------------------------------------------------
# Accessors ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_surface_column",
           def = function(sspm_object) standardGeneric("spm_surface_column")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_surface_column",
          signature("sspm_object" = "sspm_boundary"),
          function(sspm_object) sspm_object@surface_column
)

# Replacers ---------------------------------------------------------------

#' @rdname accessors-methods-sspm_boundary
#' @export
setGeneric(name = "spm_surface_column<-",
           def = function(object, value) standardGeneric("spm_surface_column<-")
)

#' @rdname accessors-methods-sspm_boundary
#' @export
setMethod("spm_surface_column<-",
          signature("object" = "sspm_boundary"),
          function(object, value) {
            object@surface_column <- value
            validObject(object)
            return(object)
          }
)
