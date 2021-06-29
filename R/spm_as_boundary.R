#' Create a sspm_boundary object
#'
#' Create a sspm_boundary object.
#'
#' @param boundaries **\[sf\]** The sf object to cast.
#' @param boundary_column **\[character\]** The column that comtains the possible
#'   subdivisions of the boundaries.
#'
#' @export
setGeneric(name = "spm_as_boundary",
           def = function(boundaries,
                          boundary_column = NULL){
             standardGeneric("spm_as_boundary")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf", boundary_column = "characterOrNULL"),
          function(boundaries, boundary_column){

            if(!checkmate::test_subset(boundary_column, names(boundaries))){
              stop("`boundary_column` must be a column of `boundaries`",
                   call. = FALSE)
            }

            boundary_object <- new("sspm_boundary",
                                   boundaries = boundaries,
                                   boundary_column = boundary_column)

            return(boundary_object)

          }
)
