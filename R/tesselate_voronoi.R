#' @export
tesselate_voronoi <- function(spaspm_object,
                              data = NULL,
                              boundaries = NULL,
                              boundary_col = "sfa",
                              min_size = 1500,
                              coords = c("lat", "lon"),
                              nb_samples = c(`4` = 10, `5` = 30, `6` = 30, `7` = 5),
                              sample_points = NULL) {

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

  # 2. Create (sample) the points
  voronoi_points <- suppressMessages(sf::st_join(data_sf, boundaries)) %>%
    dplyr::filter(!is.na(eval(dplyr::sym(boundary_col)))) %>%
    dplyr::group_by(boundary_col = eval(dplyr::sym(boundary_col))) %>%
    dplyr::filter(1:dplyr::n() %in%
                    sample(1:dplyr::n(),
                           size = nb_samples[[boundary_col[1]]]))

  return(TRUE)
  # return `spm_discrete object` OR the 2 after and build after?
  # 1. (the voronoi polygons)
  # 2. the updated `data.frame` with points.
}
