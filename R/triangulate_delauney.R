#' Perform delaunay triangulation
#'
#' Generates delaunay triangles with [ct_triangulate()][sfdct::ct_triangulate()].
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
#' @param min_size **\[numeric\]** The minimum size for a triangle above which it
#'     will be merged (in km2).
#'
#' @param seed **\[numeric\]** Passed onto [`set.seed()`][base::set.seed()],
#'     important for reproducibility of sampling.
#'
#' @inheritDotParams RTriangle::triangulate
#'
#' @return
#' A named list with three elements (each an `sf` object):
#'     * `patches`, the voronoi polygons generated
#'     * `points`, the points used for the tessellation.
#'
#' @export
triangulate_delaunay <- function(boundaries,
                                 with = NULL,
                                 boundary_column = "sfa",
                                 sample_surface = FALSE,
                                 sample_points = FALSE,
                                 nb_samples = NULL,
                                 min_size = 1000,
                                 seed = 1,
                                 ...) {

  # 1. Prep -----------------------------------------------------------------

  # Check main params
  if (is.null(boundaries)) {
    stop("boundaries argument is missing")
  } else {
    checkmate::assert_class(boundaries, "sf")
  }

  checkmate::assert_logical(sample_surface)
  checkmate::assert_logical(sample_points)
  checkmate::assert_character(boundary_column)

  if (!checkmate::test_subset(boundary_column, names(boundaries))) {
    stop("`boundary_column` must be a column of `boundaries`",
         call. = FALSE)
  }

  checkmate::assert_numeric(nb_samples, null.ok = TRUE)

  if(all(!c(sample_surface, sample_points)) && !is.null(nb_samples)){
    warning("nb_sample ignored")
  }

  checkmate::assert_numeric(min_size)
  checkmate::assert_numeric(seed)

  unique_boundaries <- unique(boundaries[[boundary_column]])
  if (length(nb_samples) == 1){
    nb_samples <- rep(nb_samples, length(unique_boundaries))
    names(nb_samples) <- unique_boundaries
  } else {
    if(any(!(names(nb_samples) %in% unique_boundaries))){
      stop("nb_samples incorrectly named")
    }
  }

  # 2. Sample points, if need be --------------------------------------------

  # Make sure seed options are set correctly
  if (getRversion() >= 3.6) suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # 2. Create (sample) the points
  boundaries_split <- split(boundaries, boundaries[[boundary_column]])

  if (sample_surface){

    sample_fun <- function(polygon, boundary_column, nb_samples){
      sf::st_sample(polygon,
                    size = nb_samples[polygon[[boundary_column]]])
    }

    set.seed(seed) ; delaunay_base <-
      lapply(boundaries_split, FUN = sample_fun,
             boundary_column = boundary_column,
             nb_samples = nb_samples) %>%
      lapply(sf::st_as_sf) %>%
      dplyr::bind_rows() %>%
      dplyr::rename(geometry = .data$x) %>%
      sf::st_join(boundaries)

  } else if (sample_points) {

    set.seed(seed) ; delaunay_base <-
      suppressMessages(sf::st_join(with, boundaries)) %>%
      dplyr::filter(!is.na(eval(dplyr::sym(boundary_column)))) %>%
      dplyr::group_by(.data[[boundary_column]]) %>%
      dplyr::filter(1:dplyr::n() %in%
                      sample(1:dplyr::n(),
                             size = nb_samples[[.data[[boundary_column]][1]]]))

  } else if (!is.null(with)) {

    delaunay_base <- with

  } else {

    delaunay_base <- boundaries %>%
      st_union() %>%
      st_convex_hull() %>%
      st_cast() %>%
      st_cast("POLYGON") %>%
      sf::st_as_sf() %>%
      dplyr::rename(geometry = .data$x) %>%
      # Need a temp col for ct_triangulate to work
      dplyr::mutate(temp_col = "temp")

  }

  # 3. Create patches -------------------------------------------------------

  delaunay_mesh <-
    suppressAll(delaunay_base %>%
                  sfdct::ct_triangulate(...))

  if("temp_col" %in% names(delaunay_mesh)){
    delaunay_mesh <- delaunay_mesh %>% dplyr::select(-"temp_col")
  }
  if("npoints" %in% names(delaunay_mesh)){
    delaunay_mesh <- delaunay_mesh %>% dplyr::select(-"npoints")
  }

  delaunay_mesh <- suppressAll(delaunay_mesh %>%
                                 sf::st_collection_extract() %>%
                                 sf::st_cast() %>%
                                 sf::st_cast("POLYGON") %>%
                                 sf::st_join(boundaries, largest = TRUE) %>%
                                 sf::st_intersection(
                                   sf::st_union(boundaries)) %>%
                                 sf::st_make_valid() %>%
                                 sf::st_cast() %>%
                                 sf::st_cast("POLYGON") %>%
                                 sf::st_make_valid() %>%
                                 dplyr::mutate(patch_id = paste0("P", 1:dplyr::n())))

  # voronoi <-
  #   suppressAll(voronoi %>%
  #                 sf::st_make_valid() %>%
  #                 dplyr::mutate(patch_id = paste("P", 1:dplyr::n(), sep = "")) %>%
  #                 dplyr::group_by(.data$patch_id, .data[[boundary_column]]) %>%
  #                 dplyr::summarize() %>%
  #                 dplyr::ungroup())

  delaunay_mesh <-
    suppressAll(dplyr::mutate(delaunay_mesh,
                              area = sf::st_area(delaunay_mesh)))
  delaunay_mesh <-
    suppressAll(dplyr::mutate(delaunay_mesh,
                              area = units::set_units(.data$area,
                                                      value = "km^2")))

  # 4. Merge small polygons -------------------------------------------------

  # TODO the removal of small polygons has not been stratified

  small_triangle <- delaunay_mesh$patch_id[which(delaunay_mesh$area <
                                                   units::set_units(min_size, value = "km^2"))]
  voronoi_edges <- suppressMessages(sf::st_intersects(delaunay_mesh))
  names(voronoi_edges) <- delaunay_mesh$patch_id

  # TODO vectorize this
  for (i in small_triangle) {
    current_triangles <- delaunay_mesh[voronoi_edges[[i]], ] %>%
      dplyr::filter(.data[[boundary_column]] ==
                      unique(.data[[boundary_column]][.data$patch_id == i])) %>%
      dplyr::filter(.data$area == max(.data$area))
    max_id <- current_triangles$patch_id
    delaunay_mesh$patch_id[delaunay_mesh$patch_id == i] <- max_id
  }

  # 5. Summarise and re - calculate area ------------------------------------

  delaunay_mesh <-
    suppressWarnings(
      suppressMessages(
        delaunay_mesh %>%
          dplyr::group_by(.data[[boundary_column]], .data$patch_id) %>%
          dplyr::summarize() %>%
          dplyr::ungroup()))
  delaunay_mesh <-
    dplyr::mutate(delaunay_mesh, area = sf::st_area(delaunay_mesh))
  delaunay_mesh <-
    dplyr::mutate(delaunay_mesh,
                  area = units::set_units(.data$area, value = "km^2"),
                  patch_id =
                    factor(paste("P", 1:dplyr::n(), sep = ""),
                           levels = paste0("P", 1:length(unique(.data$patch_id))))) %>%
    dplyr::rename(patch_area = .data$area) %>%
    dplyr::relocate(.data$patch_area, .before = .data$geometry)

  # Core function must return a list of "patches" and "points"

  if (any(c(sample_surface, sample_points))) {
    points <- delaunay_base
  } else {
    points = NULL
  }

  return(list(patches = delaunay_mesh,
              points = points))

}
