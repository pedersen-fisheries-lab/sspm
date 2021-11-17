#' Create a sspm_boundary object
#'
#' Create a sspm_boundary object.
#'
#' @param boundaries **\[sf\]** The sf object to cast.
#' @param boundary_column **\[character\]** The column that contains the possible
#'   subdivisions of the boundaries.
#' @param boundary_boundary_area_column **\[character\]** The column that contains the area
#'   of the subdivisions (optional).
#' @param patch_are a_column **\[character\]** The column that contains the area
#'   of the patches (optional).
#' @param ... further args passed onto methods
#' @param patches **\[sf\]** Patches resulting from discretization.
#' @param points **\[sf\]** Sample points used for discretization.
#'
#' @return
#' An object of class [sspm_boundary][sspm_boundary-class] or
#' [sspm_discrete_boundary][sspm_discrete_boundary-class].
#'
#' @export
setGeneric(name = "spm_as_boundary",
           def = function(boundaries,
                          boundary_column,
                          boundary_area_column,
                          patches = NULL,
                          points = NULL,
                          ...) {
             standardGeneric("spm_as_boundary")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "NULL",
                    boundary_area_column = "ANY"),
          function(boundaries, boundary_column, boundary_area_column) {
            stop("`boundary_column` cannot be NULL",
                 call. = FALSE)
          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "character",
                    boundary_area_column = "missing",
                    patches = "missing",
                    points = "missing"),
          function(boundaries, boundary_column, boundary_area_column) {

            boundaries <- boundaries %>%
              dplyr::mutate(area = sf::st_area(boundaries))
            boundaries <-
              dplyr::mutate(boundaries,
                            area = units::set_units(.data$area, value = "km^2"))
            boundary_area_column <- "area"

            spm_as_boundary(boundaries = boundaries,
                            boundary_column = boundary_column,
                            boundary_area_column = boundary_area_column)

          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "character",
                    boundary_area_column = "character",
                    patches = "missing",
                    points = "missing"),
          function(boundaries, boundary_column, boundary_area_column) {

            if (!checkmate::test_subset(boundary_column, names(boundaries))) {
              stop("`boundary_column` must be a column of `boundaries`",
                   call. = FALSE)
            }

            if (!checkmate::test_subset(boundary_area_column, names(boundaries))) {
              stop("`boundary_area_column` must be a column of `boundaries`",
                   call. = FALSE)
            }

            boundary_object <- new("sspm_boundary",
                                   boundaries = boundaries,
                                   boundary_column = boundary_column,
                                   boundary_area_column = boundary_area_column)

            return(boundary_object)

          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "character",
                    boundary_area_column = "missing",
                    patches = "ANY",
                    points = "ANY"),
          function(boundaries, boundary_column, boundary_area_column, patches, points) {

            checkmate::assert_class(patches, "sf", null.ok = TRUE)
            checkmate::assert_class(points, "sf", null.ok = TRUE)

            boundaries <- boundaries %>%
              dplyr::mutate(area = sf::st_area(boundaries))
            boundaries <-
              dplyr::mutate(boundaries,
                            area = units::set_units(.data$area, value = "km^2"))
            boundary_area_column <- "area"

            if(!(is.null(patches))){
              patches <- patches %>%
                dplyr::mutate(area = sf::st_area(patches))
              patches <-
                dplyr::mutate(patches,
                              area = units::set_units(.data$area, value = "km^2"))
            }

            spm_as_boundary(boundaries = boundaries,
                            boundary_column = boundary_column,
                            boundary_area_column = boundary_area_column,
                            patches = patches,
                            points = points)

          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "character",
                    boundary_area_column = "character"),
          function(boundaries, boundary_column, boundary_area_column, patches, points) {

            if (!checkmate::test_subset(boundary_column, names(boundaries))) {
              stop("`boundary_column` must be a column of `boundaries`",
                   call. = FALSE)
            }

            if (!checkmate::test_subset(boundary_area_column, names(boundaries))) {
              stop("`boundary_area_column` must be a column of `boundaries` OR `patches`",
                   call. = FALSE)
            }

            if (!(is.null(patches))) {

              patches <- patches %>%
                dplyr::mutate(patch_id =
                                factor(paste("P", 1:dplyr::n(), sep = ""))) %>%
                dplyr::mutate(patch_id =
                                factor(.data$patch_id,
                                       levels = paste0("P", 1:length(unique(.data$patch_id)))))
              patches <-
                dplyr::mutate(patches,
                              area = units::set_units(.data$area, value = "km^2"))

              if (!checkmate::test_subset(boundary_column, names(patches))){

                stop("`boundary_column` must be a column of `patches`",
                     call. = FALSE)

              } else {

                # TODO add option for joining instead here
                patches <- patches %>%
                  dplyr::mutate(!!boundary_column := as.factor(.data[[boundary_column]]))

              }

            }

            boundary_object <-
              new("sspm_discrete_boundary",
                  boundaries = boundaries,
                  boundary_column = boundary_column,
                  boundary_area_column = boundary_area_column,
                  method = as_discretization_method(method = I),
                  patches = patches,
                  points = points)

            return(boundary_object)

          }
)
