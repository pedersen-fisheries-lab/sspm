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
  checkmate::assert_data_frame(data)
  checkmate::assert_subset(coords, names(data))
  checkmate::assert_class(boundaries, "sf")

  # 1. Make data a sf object
  # TODO reminder that crs are assumed to be the same
  data_sf <- sf::st_as_sf(data, coords=coords,
                          crs = sf::st_crs(boundaries), remove =FALSE)

  # Make sure seed is set just before called sample
  if(getRversion()>=3.6) suppressWarnings(RNGkind(sample.kind = "Rounding"))
  set.seed(seed)

  # 2. Create (sample) the points
  if(is.null(sample_points)){
    voronoi_points <- suppressMessages(sf::st_join(data_sf, boundaries)) %>%
      dplyr::filter(!is.na(eval(dplyr::sym(boundary_col)))) %>%
      dplyr::group_by(.data[[boundary_col]]) %>%
      dplyr::filter(1:dplyr::n() %in%
                      sample(1:dplyr::n(),
                             size = nb_samples[[.data[[boundary_col]][1]]]))
  } else {
    voronoi_points <- sample_points
  }

  # 3. Create the polygons
  voronoi <-
    suppressWarnings(
      suppressMessages(
        voronoi_points %>%
          sf::st_union() %>%
          sf::st_voronoi() %>%
          sf::st_cast() %>%
          sf::st_sf() %>%
          sf::st_intersection(x=boundaries, y =.) %>%
          dplyr::mutate(voronoi_id =  paste("V", 1:dplyr::n(),sep = "")) %>%
          dplyr::group_by(voronoi_id, .data[[boundary_col]]) %>%
          dplyr::summarize() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(area_km2 = sf::st_area(.),
                        area_km2 = as.numeric(units::set_units(area_km2,
                                                               value = "km^2")))))

  # 4. Fix size
  small_voronoi <- voronoi$voronoi_id[which(voronoi$area_km2 < min_size)]
  voronoi_edges <- suppressMessages(sf::st_intersects(voronoi))
  names(voronoi_edges) <- voronoi$voronoi_id

  # TODO vectorize this
  for(i in small_voronoi){
    current_polygons <- voronoi[voronoi_edges[[i]],] %>%
      filter(.data[[boundary_col]] == .data[[boundary_col]][voronoi_id == i]) %>%
      filter(area_km2 == max(area_km2))
    max_id <- current_polygons$voronoi_id
    voronoi$voronoi_id[voronoi$voronoi_id==i] <- max_id
  }

  voronoi <-
    suppressWarnings(
      suppressMessages(
        voronoi %>%
          dplyr::group_by(.data[[boundary_col]], voronoi_id) %>%
          dplyr::summarize() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(area_km2 = sf::st_area(.),
                        area_km2 = as.numeric(units::set_units(area_km2, value = "km^2")),
                        voronoi_id = factor(paste("V", 1:n(),sep = "")))))

  # Core method must return a list of "patches" and "points"
  return(list(patches=voronoi,
              points = voronoi_points))
}
