#' Create a `spaspm` model object
#'
#' The starting point of the `spaspm` workflow: creating a base `spaspm` object.
#'
#' @param model_name **\[character\]** The name to be given to the model
#' @param boundaries **\[sf\]** The spatial boundaries (polygons) for the model.
#' @inheritParams as_spaspm_data
#' @param ... TBD
#'
#' @return
#' An object of class  [spaspm][spaspm-class].
#'
#' @rdname spaspm-constructor
#' @export
spaspm <- function(model_name = "My SPASPM model",
                   name = "Biomass",
                   data,
                   uniqueID,
                   boundaries,
                   coords = NULL,
                   ...){

  # TODO CRS checks

  # 1. Ingest data and perform the correct checks
  the_spapspm_data <- as_spaspm_data(data = data,
                                     coords = coords,
                                     name = name,
                                     uniqueID = uniqueID,
                                     crs = sf::st_crs(boundaries),
                                     ...)

  # 2. Create basis spaspm object
  the_object <- new("spaspm",
                    name = model_name,
                    data = the_spapspm_data,
                    boundaries = boundaries)

  return(the_object)
}


