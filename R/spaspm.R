# Model Object Constructor
#' @export
spaspm <- function(name="SPSPM Model", data, boundaries){

  # Check name to be character and data to be a data.frame/tibble
  checkmate::assert_class(name, "character")
  checkmate::assert_class(data, c("data.frame"))
  checkmate::assert_class(boundaries, c("sf", "sfc", "sfc_POLYGON",
                                       "sfc_MULTIPOLYGON"))

  new("spaspm", name = name, data = data, boundaries = boundaries)
}
