# Model Object Constructor
#' @export
spaspm <- function(name="SPSPM Model", data, boundaries){

  # Check name to be character and data to be a data.frame/tibble
  checkmate::check_class(name, "character")
  checkmate::check_class(data, c("data.frame"))

  checkmate::check_class(boundaries, c("sf", "sfc", "sfc_POLYGON",
                                       "sfc_MULTIPOLYGON"))

  new("spaspm", name = name, data = data, boundaries = boundaries)
}
