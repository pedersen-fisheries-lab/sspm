#' Perform delaunay triangulation
#'
#' Generates delaunay triangles with [ct_triangulate()][sfdct::ct_triangulate()].
#'
#' @param boundaries **\[sf\]** The boundaries to be used.
#' @param with **\[sf\]** A set of data points to use for voronoisation.
#' @param boundary **\[character\]** The column in `boundaries` that is to
#'     be used for the stratified sampling.
#'
#' @param sample_surface **\[logical]** Whether to sample the surfaces in
#'     `boundaries`, Default to `FALSE`.
#' @param sample_points **\[logical]** Whether to sample points from `with` or
#'     to take all points in `with`. Default to `TRUE`.
#'
#' @param nb_samples **\[named character vector\]** The number of samples to draw
#'     by boundary polygons (must bear the levels of `boundary` as names
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
                                 boundary = "sfa",
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
  checkmate::assert_character(boundary)

  if (!checkmate::test_subset(boundary, names(boundaries))) {
    stop("`boundary` must be a column of `boundaries`",
         call. = FALSE)
  }

  checkmate::assert_numeric(nb_samples, null.ok = TRUE)

  if(all(!c(sample_surface, sample_points)) && !is.null(nb_samples)){
    warning("nb_sample ignored")
  }

  checkmate::assert_numeric(min_size)
  checkmate::assert_numeric(seed)

  unique_boundaries <- unique(boundaries[[boundary]])

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
  boundaries_split <- split(boundaries, boundaries[[boundary]])

  if (sample_surface){

    delaunay_base <- sample_points(mode = "surface", with,
                                   boundaries, boundary, nb_samples, seed)

  } else if (sample_points) {

    delaunay_base <- sample_points(mode = "points", with,
                                   boundaries, boundary, nb_samples, seed)

  } else if (!is.null(with)) {

    stopifnot(sf::st_is(with, "POINT"))

    delaunay_base <- with

  } else {

    delaunay_base <- make_base_from_bounds(boundaries) %>%
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

  delaunay_mesh <-
    suppressAll(delaunay_mesh %>%
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

  delaunay_mesh <-
    suppressAll(dplyr::mutate(delaunay_mesh,
                              area = sf::st_area(delaunay_mesh)))
  delaunay_mesh <-
    suppressAll(dplyr::mutate(delaunay_mesh,
                              area = units::set_units(.data$area,
                                                      value = "km^2")))

  # 4. Merge small polygons -------------------------------------------------

  delaunay_mesh <- merge_small_polygons(delaunay_mesh, min_size, boundary)

  # 5. Summarise and re - calculate area ------------------------------------

  delaunay_mesh <- cleanup_polygons(delaunay_mesh, boundary)

  return(list(patches = delaunay_mesh,
              points = points))

}

# Helpers -----------------------------------------------------------------
