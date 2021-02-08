#' Create a spaspm model object
#'
#' TODO
#'
#' @rdname spaspm-constructor
#' @export
spaspm <- function(model_name = "My SPASPM model",
                   dataset_name = "Biomass",
                   data,
                   uniqueID,
                   boundaries,
                   coords = NULL,
                   ...){

  # TODO CRS checks

  # 1. Ingest data and perform the correct checks
  the_spapspm_data <- as_spaspm_data(data, coords, dataset_name, uniqueID,
                                     crs = sf::st_crs(boundaries), ...)

  # 2. Create basis spaspm object
  the_object <- new("spaspm",
                    name = model_name,
                    data = the_spapspm_data,
                    boundaries = boundaries)

  return(the_object)
}


