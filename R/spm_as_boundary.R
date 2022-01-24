#' Create a sspm_boundary object
#'
#' Create a sspm_boundary object.
#'
#' @param boundaries **\[sf\]** The sf object to cast.
#' @param boundary_column **\[character\]** The column that contains the possible
#'   subdivisions of the boundaries.
#' @param patches **\[sf\]** Patches resulting from discretization.
#' @param points **\[sf\]** Sample points used for discretization.
#' @param boundary_area_column **\[character\]** The column that contains the area
#'   of the subdivisions (optional).
#' @param patch_area_column **\[character\]** The column that contains the area
#'   of the patches (optional).
#'
#' @return
#' An object of class [sspm_boundary][sspm_boundary-class] or
#' [sspm_discrete_boundary][sspm_discrete_boundary-class].
#'
#' @export
setGeneric(name = "spm_as_boundary",
           def = function(boundaries,
                          boundary_column,
                          patches = NULL,
                          points = NULL,
                          boundary_area_column = NULL,
                          patch_area_column = NULL) {
             standardGeneric("spm_as_boundary")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "missing"),
          function(boundaries, boundary_column, patches, points,
                   boundary_area_column, patch_area_column) {
            stop("`boundaries` cannot be missing",
                 call. = FALSE)
          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundary_column = "missing"),
          function(boundaries, boundary_column, patches, points,
                   boundary_area_column, patch_area_column) {
            stop("`boundary_column` cannot be missing",
                 call. = FALSE)
          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary_column = "character",
                    patches = "missing",
                    points = "missing"),
          function(boundaries, boundary_column, patches, points,
                   boundary_area_column, patch_area_column) {

            boundaries_list <- check_boundaries(boundaries, boundary_column,
                                                boundary_area_column)

            boundaries <- boundaries_list$features
            boundary_area_column <- boundaries_list$column

            boundary_object <-
              new("sspm_boundary",
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
                    patches = "ANY",
                    points = "ANY"),
          function(boundaries, boundary_column, patches, points,
                   boundary_area_column, patch_area_column) {

            checkmate::assert_class(patches, "sf", null.ok = TRUE)
            checkmate::assert_class(points, "sf", null.ok = TRUE)

            # Boundaries
            boundaries_list <- check_boundaries(boundaries, boundary_column,
                                                boundary_area_column)

            boundaries <- boundaries_list$features
            boundary_area_column <- boundaries_list$column

            # Patches
            ## TODO patches vs points provision

            patches_list <- check_patches(patches,
                                          patch_area_column)

            patches <- patches_list$features
            patch_area_column <- patches_list$column

            patches <- patches %>%
              dplyr::mutate(patch_id =
                              factor(paste("P", 1:dplyr::n(), sep = ""))) %>%
              dplyr::mutate(patch_id =
                              factor(.data$patch_id,
                                     levels = paste0("P", 1:length(unique(.data$patch_id))))) %>%
              dplyr::relocate("patch_id", .after = boundary_column) %>%
              # TODO add option for joining here as well
              dplyr::mutate(!!boundary_column := as.factor(.data[[boundary_column]]))

            # Points
            ## TODO

            boundary_object <-
              new("sspm_discrete_boundary",
                  boundaries = boundaries,
                  boundary_column = boundary_column,
                  boundary_area_column = boundary_area_column,
                  method = as_discretization_method(method = I),
                  patches_area_column = patch_area_column,
                  patches = patches,
                  points = points)

          }
)

# -------------------------------------------------------------------------

check_boundaries <- function(boundaries, boundary_column,
                             boundary_area_column){

  checkmate::assert_class(boundaries, "sf")
  checkmate::assert_class(boundary_column, "character")
  checkmate::assert_class(boundary_area_column, "character", null.ok = TRUE)

  if (!checkmate::test_subset(boundary_column, names(boundaries))) {
    stop("`boundary_column` must be a column of `boundaries`",
         call. = FALSE)
  }

  # Ensure boundaries are factors
  boundaries[[boundary_column]] <- as.factor(boundaries[[boundary_column]])

  new_boundary_area_column <- paste0("area_", boundary_column)

  if(!is.null(boundary_area_column)){

    if (!checkmate::test_subset(boundary_area_column, names(boundaries))) {
      stop("`boundary_area_column` must be a column of `boundaries`",
           call. = FALSE)
    }

    cli::cli_alert_warning("SSPM assumes areas are supplied in km^2")

    boundaries <-
      dplyr::mutate(boundaries,
                    !!boundary_area_column := units::set_units(.data[[boundary_area_column]],
                                                              value = "km^2")) %>%
      dplyr::rename(!!new_boundary_area_column := .data$boundary_area_column)

  } else {

    boundaries <- calculate_spatial_feature_areas(boundaries) %>%
      dplyr::rename(!!new_boundary_area_column := .data$area)

  }

  boundary_list <- list(features = boundaries,
                        column = new_boundary_area_column)

  return(boundary_list)

}

check_patches <- function(patches,
                          patches_area_column){

  checkmate::assert_class(patches, "sf")
  checkmate::assert_class(patches_area_column, "character", null.ok = TRUE)

  if(!is.null(patches_area_column)){

    if (!checkmate::test_subset(patches_area_column, names(patches))) {
      stop("`boundary_area_column` must be a column of `boundaries`",
           call. = FALSE)
    }

    cli::cli_alert_warning("SSPM assumes areas are supplied in km^2")

    patches <-
      dplyr::mutate(patches,
                    !!patches_area_column := units::set_units(.data[[patches_area_column]],
                                                              value = "km^2"))

  } else {

    patches <- calculate_spatial_feature_areas(patches) %>%
      dplyr::rename(patch_area = .data$area)
    patches_area_column <- "patch_area"

  }

  patches_list <- list(features = patches,
                       column = patches_area_column)

  return(patches_list)

}

calculate_spatial_feature_areas <- function(features){

  checkmate::assert_class(features, "sf")

  features <- features %>%
    dplyr::mutate(area = sf::st_area(features))

  features <-
    dplyr::mutate(features,
                  area = units::set_units(.data$area, value = "km^2"))

  return(features)
}
