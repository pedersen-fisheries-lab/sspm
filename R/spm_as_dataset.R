#' Create a `sspm_dataset` dataset structure
#'
#' This casts a `data.frame` or `sf` object into  an object of class
#' [`sspm_dataset`][sspm_dataset-class].
#'
#' @param data **\[data.frame OR sf\]** The dataset.
#' @param time_column **\[character\]** The column of `data` for the temporal
#'     dimensions (i.e. year).
#' @param coords **\[character\]** The column of `data` for longitude and
#'     latitude of the observations.
#' @param name **\[character\]** The name of the dataset, default to "Biomass".
#' @param uniqueID **\[character\]** The column of `data` that is unique for all
#'     rows of the data matrix.
#' @param crs Coordinate reference system, passed onto [st_as_sf][sf].
#'
#' @return
#' An object of class [`sspm_dataset`][sspm_dataset-class].
#'
#' @export
setGeneric(name = "spm_as_dataset",
           def = function(data, name, time_column, uniqueID, coords = NULL, crs = NULL) {

             if (!checkmate::test_subset(uniqueID, names(data))) {
               stop("`uniqueID` must be a column of `data`", call. = FALSE)
             }

             if (!(length(unique(data[[uniqueID]])) == nrow(data))) {
               stop("`uniqueID` must be unique for each row of `data`", call. = FALSE)
             }

             if (!checkmate::test_subset(time_column, names(data))) {
               stop("`time_column` must be a column of `data`", call. = FALSE)
             }

             standardGeneric("spm_as_dataset")
           }
)

# Methods -----------------------------------------------------------------

#' @rdname spm_as_dataset
#' @export
setMethod(f = "spm_as_dataset",
          signature(data = "data.frame", coords = "missingOrNULL"),
          function(data, name, time_column, uniqueID, coords, crs) {

            stop("Argument `coords` must be provided when data matrix is a dataframe",
                 call. = FALSE)
          }
)

# If data.frame with coords, make it sf
#' @rdname spm_as_dataset
#' @export
setMethod(f = "spm_as_dataset",
          signature(data = "data.frame", coords = "list"),
          function(data, name, time_column, uniqueID, coords, crs) {
            coords <- unlist(coords)
            spm_as_dataset(data, name, time_column, uniqueID, coords, crs)
          }
)


# If data.frame with coords, make it sf
#' @rdname spm_as_dataset
#' @export
setMethod(f = "spm_as_dataset",
          signature(data = "data.frame", coords = "character"),
          function(data, name, time_column, uniqueID, coords, crs) {

            # Check coords
            if (!checkmate::test_subset(coords, names(data))) {
              stop("`coords` must be columns of `data`", call. = FALSE)
            }
            if (length(coords) != 2) {
              stop("`coords` must be of length 2", call. = FALSE)
            }

            # From a data.frame and coords, cast as sf (keep columns)
            info_message <-
              paste0(" Casting data matrix into simple feature collection using columns: ",
                     paste(cli::col_green(coords), collapse = ", "))
            cli::cli_alert_info(info_message)

            # TODO better CRS checks
            if (is.null(crs)) {
              info_message <-
                paste0(" Warning: sspm is assuming WGS 84 CRS is to be ",
                       "used for casting")
              cli::cli_alert_warning(info_message)
              crs <- sf::st_crs(4326)
            }

            new_data <- sf::st_as_sf(x = data, coords = coords, crs = crs,
                                     remove = FALSE)

            the_sspm_dataset <- new("sspm_dataset",
                                    name = name,
                                    data = new_data,
                                    time_column = time_column,
                                    uniqueID = uniqueID,
                                    coords = coords)

            return(the_sspm_dataset)
          }
)

# If sf, ingest as is
#' @rdname spm_as_dataset
#' @export
setMethod(f = "spm_as_dataset",
          signature(data = "sf", coords = "ANY"),
          function(data, name, time_column, uniqueID, coords, crs) {

            # Test if point
            if (any(sf::st_is(data, "POINT"))) {

              the_sspm_dataset <- new("sspm_dataset",
                                      name = name,
                                      data = data,
                                      time_column = time_column,
                                      uniqueID = uniqueID,
                                      coords = coords)

            } else if(any(sf::st_is(data, "POLYGON")) ||
                      any(sf::st_is(data, "MULTIPOLYGON"))) {

              # Create boundaries, create patch id
              patches <- data %>%
                dplyr::select("geometry") %>%
                dplyr::distinct() %>%
                dplyr::mutate(patch_id = paste0("P", 1:dplyr::n()))

              boundary_data <- patches %>%
                sf::st_union() %>%
                sf::st_as_sf() %>%
                dplyr::mutate(boundary_col = "B1") %>%
                dplyr::rename(geometry = .data$x)

              boundaries <- spm_as_boundary(boundaries = boundary_data,
                                            boundary_column = "boundary_col",
                                            discrete = TRUE,
                                            patches = patches,
                                            points = NULL)

              the_sspm_dataset <- new("sspm_dataset",
                                      name = name,
                                      data = data,
                                      time_column = time_column,
                                      uniqueID = uniqueID,
                                      coords = coords,
                                      is_mapped = TRUE,
                                      boundaries = boundaries)

            } else {

              stop("sf object must be of type POINT or POLYGON to be sspm datasets")

            }

            return(the_sspm_dataset)
          }
)
