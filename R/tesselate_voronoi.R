#' Perform voronoi tesselation
#'
#' Generates voronoi polygons by first performing stratified sampling across
#' boundary polygons, then by running  the voronoisation with
#' [st_voronoi()][sf::st_voronoi()].
#'
#' @param boundaries **\[sf\]** The boundaries to be used.
#' @param with **\[sf\]** A set of data points to use for voronoisation.
#' @param boundary_column **\[character\]** The column in `boundaries` that is to
#'     be used for the stratified sampling.
#'
#' @param sample_surface **\[logical]** Whether to sample the surfaces in
#'     `boundaries`, Default to `FALSE`.
#' @param sample_points **\[logical]** Whether to sample points from `with` or
#'     to take all points in `with`. Default to `TRUE`.
#'
#' @param nb_samples **\[named character vector\]** The number of samples to draw
#'     by boundary polygons (must bear the levels of `boundary_column` as names
#'     or be a single value to be applied to each level).
#' @param min_size **\[numeric\]** The minimum size for a polygon above which it
#'     will be merged (in km2).
#' @param stratify **\[logical]** Whether the discretization happens within the
#'     boundaries or whether the whole area is to be used (default to TRUE).
#'
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
                              boundary_column = "sfa",
                              sample_surface = FALSE,
                              sample_points = TRUE,
                              nb_samples,
                              min_size = 1500,
                              stratify = TRUE,
                              seed = 1) {

  # TODO to be discussed
  # 1. NAFO division

  # Prep --------------------------------------------------------------------

  # Check main params
  if (is.null(boundaries)) {
    stop("boundaries argument is missing")
  } else {
    checkmate::assert_class(boundaries, "sf")
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

  unique_boundaries <- unique(boundaries[[boundary_column]])
  if (length(nb_samples) == 1){
    nb_samples <- rep(nb_samples, length(unique_boundaries))
    names(nb_samples) = unique_boundaries
  } else {
    if(any(!(names(nb_samples) %in% unique_boundaries))){
      stop("nb_samples incorrectly named")
    }
  }

  # Body --------------------------------------------------------------------

  # Make sure seed options are set correctly
  if (getRversion() >= 3.6) suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # 2. Create (sample) the points
  boundaries_split <- split(boundaries, boundaries[[boundary_column]])

  if (sample_surface){

    sample_fun <- function(polygon, boundary_column, nb_samples){
      sf::st_sample(polygon,
                    size = nb_samples[polygon[[boundary_column]]])
    }

    set.seed(seed) ; voronoi_points <-
      lapply(boundaries_split, FUN = sample_fun,
             boundary_column = boundary_column,
             nb_samples = nb_samples) %>%
      lapply(sf::st_as_sf) %>%
      dplyr::bind_rows() %>%
      dplyr::rename(geometry = .data$x) %>%
      sf::st_join(boundaries)

  } else {

    if (sample_points) {

      set.seed(seed) ; voronoi_points <-
        suppressMessages(sf::st_join(with, boundaries)) %>%
        dplyr::filter(!is.na(eval(dplyr::sym(boundary_column)))) %>%
        dplyr::group_by(.data[[boundary_column]]) %>%
        dplyr::filter(1:dplyr::n() %in%
                        sample(1:dplyr::n(),
                               size = nb_samples[[.data[[boundary_column]][1]]]))
    } else {

      voronoi_points <- with

    }

  }

  # 3. Create the polygons
  if(stratify){

    envelopes <- boundaries_split %>%
      lapply(function(x) { x[["geometry"]] } )

    voronoi <- voronoi_points %>%
      split(voronoi_points[[boundary_column]]) %>%
      lapply(function(x) { suppressAll(sf::st_union(x)) } ) %>%
      mapply(FUN = function(x, y) {
        suppressAll(sf::st_voronoi(x, envelope = y)) },
        envelopes, SIMPLIFY = FALSE) %>%
      lapply(function(x) { suppressAll(sf::st_cast(x)) } ) %>%
      lapply(function(x) { suppressAll(sf::st_sf(x)) } ) %>%
      mapply(FUN = function(x, y) { suppressAll(sf::st_intersection(x, y)) },
             boundaries_split, SIMPLIFY = FALSE) %>%
      dplyr::bind_rows()

    voronoi <- sf::st_sf(voronoi) %>%
      dplyr::rename(geometry = .data$x)

  } else {

    voronoi <-
      suppressAll(voronoi_points %>%
                    sf::st_union() %>%
                    sf::st_voronoi() %>%
                    sf::st_cast() %>%
                    sf::st_sf())

    voronoi <-
      suppressAll(sf::st_intersection(x = boundaries, y = voronoi))

  }

  voronoi <-
    suppressAll(voronoi %>%
                  sf::st_make_valid() %>%
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

  # 4. Merge small polygons
  # TODO the removal of small polygons has not been stratified

  small_voronoi <- voronoi$patch_id[which(voronoi$area <
                                            units::set_units(min_size, value = "km^2"))]
  voronoi_edges <- suppressMessages(sf::st_intersects(voronoi))
  names(voronoi_edges) <- voronoi$patch_id

  # TODO vectorize this
  for (i in small_voronoi) {
    current_polygons <- voronoi[voronoi_edges[[i]], ] %>%
      dplyr::filter(.data[[boundary_column]] ==
                      unique(.data[[boundary_column]][.data$patch_id == i])) %>%
      dplyr::filter(.data$area == max(.data$area))
    max_id <- current_polygons$patch_id
    voronoi$patch_id[voronoi$patch_id == i] <- max_id
  }

  # -------------------------------------------------------------------------

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
