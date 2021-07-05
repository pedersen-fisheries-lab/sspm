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
                          boundary_column,
                          surface_column){
             standardGeneric("spm_as_boundary")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "NULL",
                    surface_column = "ANY"),
          function(boundaries, boundary_column, surface_column){
            stop("`boundary_column` cannot be NULL",
                 call. = FALSE)
          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "character",
                    surface_column = "missing"),
          function(boundaries, boundary_column, surface_column){

            boundaries <- boundaries %>%
              dplyr::mutate(area = sf::st_area(boundaries))
            surface_column <- "area"

            spm_as_boundary(boundaries = boundaries,
                            boundary_column = boundary_column,
                            surface_column = surface_column)

          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "character",
                    surface_column = "character"),
          function(boundaries, boundary_column, surface_column){

            if(!checkmate::test_subset(boundary_column, names(boundaries))){
              stop("`boundary_column` must be a column of `boundaries`",
                   call. = FALSE)
            }

            if(!checkmate::test_subset(surface_column, names(boundaries))){
              stop("`surface_column` must be a column of `boundaries`",
                   call. = FALSE)
            }

            boundary_object <- new("sspm_boundary",
                                   boundaries = boundaries,
                                   boundary_column = boundary_column,
                                   surface_column = surface_column)

            return(boundary_object)

          }
)
