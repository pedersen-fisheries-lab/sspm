#' Perform voronoi tesselation
#'
#' Generates voronoi polygons by first performing stratified sampling across
#' boundary polygons, then by running  the voronoisation with
#' [st_voronoi()][sf::st_voronoi()]. The sampling can be bypassed by providing
#' points via `sample_points`.
#'
#' @param spaspm_object **\[spaspm\]** `spaspm` object to be discretized.
#' @param data **\[data.frame\]** Overwrites the data slot of `spaspm_object`.
#' @param boundaries **\[sf\]** Overwrites the boundaries slot of `spaspm_object`.
#' @param boundary_col **\[character\]** The column in `boundaries` that is to
#'     be used for the stratified sampling.
#' @param nb_samples **\[named character vector\]** The number of samples to draw
#'     by boundary polygons (must bear the levels of `boundary_col` as names).
#' @param min_size **\[numeric\]** The minimum size for a polygon above which it
#'     will be merged (in km2).
#' @param coords **\[character vector\]** The columns in `data` (or the `data`
#'     slot of `spaspm_object`) that contains the coordinates (lat, long) of the
#'     individual observations.
#' @param sample_points **\[sf\]** A set of points to use for voronoisation,
#'     all parameters used for sampling are ignored.
#' @param seed **\[numeric\]** Passed onto [`set.seed()`][base::set.seed()],
#'     important for reproducibility of sampling.
#'
#' @return
#' A named list with three elements (each an `sf` object):
#'     * `data_spatial`, the spatialized version of `data`
#'     * `patches`, the voronoi polygons generated
#'     * `points`, the points used for the tessellation.
#'
#' @export
tesselate_voronoi <- function(spaspm_object,
                              data = NULL,
                              boundaries = NULL,
                              boundary_col = "sfa",
                              nb_samples = c(`4` = 10, `5` = 30, `6` = 30, `7` = 5),
                              min_size = 1500,
                              coords = c("lat", "lon"),
                              sample_points = NULL,
                              seed = 1) {

  # TODO some steps in original code not supported as these are not general:
  # To be discussed
  # 1. breaking of big polygons
  # 2. NAFO division

  # Prep --------------------------------------------------------------------

  # Check main params
  checkmate::assert_class(spaspm_object, "spaspm")
  checkmate::assert_character(coords)
  checkmate::assert_numeric(nb_samples)
  if (!checkmate::test_null(sample_points)) {
    checkmate::assert_class(sample_points, "sf")
  }

  # Get params from model object if necessary
  name <- spm_name(spaspm_object)
  data <- if (is.null(data)) spm_data(spaspm_object) else data
  boundaries <- if (is.null(boundaries)) spm_boundaries(spaspm_object) else boundaries

  # Check these params as well
  checkmate::assert_class(data, "spaspm_data")
  checkmate::assert_class(boundaries, "sf")

  # Body --------------------------------------------------------------------

  # 1. Make data a sf object
  # TODO review accessors and use them here
  data_spatial <- data@data

  # Make sure seed is set just before called sample
  if(getRversion()>=3.6) suppressWarnings(RNGkind(sample.kind = "Rounding"))
  set.seed(seed)

  # 2. Create (sample) the points
  if(is.null(sample_points)){
    voronoi_points <- suppressMessages(sf::st_join(data_spatial, boundaries)) %>%
      dplyr::filter(!is.na(eval(dplyr::sym(boundary_col)))) %>%
      dplyr::group_by(.data[[boundary_col]]) %>%
      # TODO revise here: allows stratified sampling, but doesn't allow a given
      # number (like "want to sample 100" polygons)
      dplyr::filter(1:dplyr::n() %in%
                      sample(1:dplyr::n(),
                             size = nb_samples[[.data[[boundary_col]][1]]]))
  } else {
    voronoi_points <- sample_points
  }

  # 3. Create the polygons
  voronoi <-
    suppressAll(voronoi_points %>%
                  sf::st_union() %>%
                  sf::st_voronoi() %>%
                  sf::st_cast() %>%
                  sf::st_sf())
  voronoi <-
    suppressAll(sf::st_intersection(x=boundaries, y = voronoi))
  voronoi <-
    suppressAll(voronoi %>%
                  dplyr::mutate(voronoi_id =  paste("V", 1:dplyr::n(),sep = "")) %>%
                  dplyr::group_by(.data$voronoi_id, .data[[boundary_col]]) %>%
                  dplyr::summarize() %>%
                  dplyr::ungroup())
  voronoi <-
    suppressAll(dplyr::mutate(voronoi,
                              area_km2 = sf::st_area(voronoi)))
  voronoi <-
    suppressAll(dplyr::mutate(voronoi,
                              area_km2 = as.numeric(units::set_units(.data$area_km2,
                                                                     value = "km^2"))))

  # 4. Fix size
  small_voronoi <- voronoi$voronoi_id[which(voronoi$area_km2 < min_size)]
  voronoi_edges <- suppressMessages(sf::st_intersects(voronoi))
  names(voronoi_edges) <- voronoi$voronoi_id

  # TODO vectorize this
  for(i in small_voronoi){
    current_polygons <- voronoi[voronoi_edges[[i]],] %>%
      dplyr::filter(.data[[boundary_col]] == .data[[boundary_col]][.data$voronoi_id == i]) %>%
      dplyr::filter(.data$area_km2 == max(.data$area_km2))
    max_id <- current_polygons$voronoi_id
    voronoi$voronoi_id[voronoi$voronoi_id==i] <- max_id
  }

  # Summarise and calculate area
  voronoi <-
    suppressWarnings(
      suppressMessages(
        voronoi %>%
          dplyr::group_by(.data[[boundary_col]], .data$voronoi_id) %>%
          dplyr::summarize() %>%
          dplyr::ungroup()))
  voronoi <-
    dplyr::mutate(voronoi, area_km2 = sf::st_area(voronoi))
  voronoi <-
    dplyr::mutate(voronoi,
                  area_km2 = as.numeric(units::set_units(.data$area_km2, value = "km^2")),
                  voronoi_id = factor(paste("V", 1:dplyr::n(),sep = "")))

  # Core function must return a list of "patches" and "points"
  return(list(data_spatial = data_spatial,
              patches=voronoi,
              points = voronoi_points))
}
