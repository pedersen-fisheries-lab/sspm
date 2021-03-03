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
                   time_col,
                   uniqueID,
                   boundaries,
                   coords = NULL,
                   ...){

  # TODO better CRS checks
  if (is.null(crs)){
    info_message <-
      paste0(" Warning: spaspm is assuming that the CRS of boundaries is to be ",
             "used for casting")
    cli::cli_alert_warning(info_message)
    crs <- sf::st_crs(boundaries)
  }

  # 1. Ingest data and perform the correct checks
  the_spapspm_data <- as_spaspm_data(data = data,
                                     time_col = time_col,
                                     coords = coords,
                                     name = name,
                                     uniqueID = uniqueID,
                                     crs = crs,
                                     ...)

  # 2. Create basis spaspm object
  the_object <- new("spaspm",
                    name = model_name,
                    data = the_spapspm_data,
                    boundaries = boundaries)

  return(the_object)
}


