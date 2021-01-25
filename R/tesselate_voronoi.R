#' @export
tesselate_voronoi <- function(spaspm_object,
                              coords = c("lat", "lon"),
                              nb_samples = 30,
                              sample_points = NULL){

  print(coords)
  print(nb_samples)
  print(sample_points)

  checkmate::assert_class(spaspm_object, "spaspm")

  checkmate::assert_character(coords)
  checkmate::assert_numeric(nb_samples)

  if(!checkmate::test_null(sample_points)){
    checkmate::assert_class(sample_points, "sf")
  }


  #
  # return 2 `sf object` :
  # 1. (the voronoi polygons)
  # 2. the updated `data.frame` with points.
}
