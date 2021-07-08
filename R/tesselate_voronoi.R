#' Perform voronoi tesselation
#'
#' Generates voronoi polygons by first performing stratified sampling across
#' boundary polygons, then by running  the voronoisation with
#' [st_voronoi()][sf::st_voronoi()].
#'
#' @param boundaries **\[sf\]** The boundaries to be used.
#' @param with **\[sf\]** A set of data points to use for voronoisation.
#' @param sample_points **\[logical]** Whether to sample points from `with` or
#'     to take all points in `with`. Default to `TRUE`.
#' @param boundary_column **\[character\]** The column in `boundaries` that is to
#'     be used for the stratified sampling.
#' @param nb_samples **\[named character vector\]** The number of samples to draw
#'     by boundary polygons (must bear the levels of `boundary_column` as names).
#' @param min_size **\[numeric\]** The minimum size for a polygon above which it
#'     will be merged (in km2).
#' @param seed **\[numeric\]** Passed onto [`set.seed()`][base::set.seed()],
#'     important for reproducibility of sampling.
#'
#' @return
#' A named list with three elements (each an `sf` object):
#'     * `patches`, the voronoi polygons generated
#'     * `points`, the points used for the tessellation.
#'
#' @export
tesselate_voronoi <- function(boundaries,
                              with,
                              sample_points = TRUE,
                              boundary_column = "sfa",
                              nb_samples = c(`4` = 10, `5` = 30,
                                             `6` = 30, `7` = 5),
                              min_size = 1500,
                              seed = 1) {

  # TODO some steps in original code not supported as these are not general:
  # To be discussed
  # 1. breaking of big polygons
  # 2. NAFO division

  # Prep --------------------------------------------------------------------

  # Check main params
  if (is.null(boundaries)) {
    stop("boundaries argument is missing")
  } else {
    checkmate::assert_class(boundaries, "sf")
  }

  if (is.null(with)) {
    stop("with argument is missing")
  } else {
    checkmate::assert_class(with, "sf")
  }

  checkmate::assert_logical(sample_points)
  checkmate::assert_character(boundary_column)

  if (!checkmate::test_subset(boundary_column, names(boundaries))) {
    stop("`boundary_column` must be a column of `boundaries`",
         call. = FALSE)
  }

  checkmate::assert_numeric(nb_samples)
  checkmate::assert_numeric(min_size)
  checkmate::assert_numeric(seed)

  # Body --------------------------------------------------------------------

  # Make sure seed options are set correctly
  if (getRversion() >= 3.6) suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # 2. Create (sample) the points
  if (sample_points) {
    set.seed(seed) ; voronoi_points <-
      suppressMessages(sf::st_join(with, boundaries)) %>%
      dplyr::filter(!is.na(eval(dplyr::sym(boundary_column)))) %>%
      dplyr::group_by(.data[[boundary_column]]) %>%
      # TODO revise here: allows stratified sampling, but doesn't allow a given
      # number (like "want to sample 100" polygons)
      dplyr::filter(1:dplyr::n() %in%
                      sample(1:dplyr::n(),
                             size = nb_samples[[.data[[boundary_column]][1]]]))
  } else {
    voronoi_points <- with
  }

  # 3. Create the polygons
  voronoi <-
    suppressAll(voronoi_points %>%
                  sf::st_union() %>%
                  sf::st_voronoi() %>%
                  sf::st_cast() %>%
                  sf::st_sf())
  voronoi <-
    suppressAll(sf::st_intersection(x = boundaries, y = voronoi))
  voronoi <-
    suppressAll(voronoi %>%
                  dplyr::mutate(patch_id = paste("V", 1:dplyr::n(), sep = "")) %>%
                  dplyr::group_by(.data$patch_id, .data[[boundary_column]]) %>%
                  dplyr::summarize() %>%
                  dplyr::ungroup())
  voronoi <-
    suppressAll(dplyr::mutate(voronoi,
                              area = sf::st_area(voronoi)))
  voronoi <-
    suppressAll(dplyr::mutate(voronoi,
                              area = units::set_units(.data$area,
                                                      value = "km^2")))

  # 4. Fix size
  small_voronoi <- voronoi$patch_id[which(voronoi$area <
                                            units::set_units(min_size, value = "km^2"))]
  voronoi_edges <- suppressMessages(sf::st_intersects(voronoi))
  names(voronoi_edges) <- voronoi$patch_id

  # TODO vectorize this
  for (i in small_voronoi) {
    current_polygons <- voronoi[voronoi_edges[[i]], ] %>%
      dplyr::filter(.data[[boundary_column]] == .data[[boundary_column]][.data$patch_id == i]) %>%
      dplyr::filter(.data$area == max(.data$area))
    max_id <- current_polygons$patch_id
    voronoi$patch_id[voronoi$patch_id == i] <- max_id
  }

  # Summarise and calculate area
  voronoi <-
    suppressWarnings(
      suppressMessages(
        voronoi %>%
          dplyr::group_by(.data[[boundary_column]], .data$patch_id) %>%
          dplyr::summarize() %>%
          dplyr::ungroup()))
  voronoi <-
    dplyr::mutate(voronoi, area = sf::st_area(voronoi))
  voronoi <-
    dplyr::mutate(voronoi,
                  area = units::set_units(.data$area, value = "km^2"),
                  patch_id = factor(paste("V", 1:dplyr::n(), sep = "")))

  # Core function must return a list of "patches" and "points"
  return(list(patches = voronoi,
              points = voronoi_points))
}
