#' @export
tesselate_voronoi <- function(sapspm_model, data, boundaries,
                              long, lat,
                              nb_samples = NULL,
                              sample_points = NULL){

  checkmate::assert_class(sapspm_model, "spaspm")
  checkmate::assert_class(data, c("data.frame"))
  checkmate::assert_character(long)
  checkmate::assert_numeric(lat)
  checkmate::assert_class(boundaries, "sf")
  checkmate::assert_numeric(nb_samples)
  checkmate::assert_class(sample_points, "sf")

  # return 2 `sf object` :
  # 1. (the voronoi polygons)
  # 2. the updated `data.frame` with points.
}
