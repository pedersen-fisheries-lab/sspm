#' Create a `sspm` model object
#'
#' The starting point of the `sspm` workflow: creating a base `sspm` object.
#'
#' @param model_name **\[character\]** The name to be given to the model
#' @param boundaries **\[sf\]** The spatial boundaries (polygons) for the model.
#' @inheritParams as_sspm_data
#' @param ... TBD
#'
#' @return
#' An object of class  [sspm][sspm-class].
#'
#' @rdname sspm-constructor
#' @export
sspm <- function(model_name = "My sspm model",
                 name = "Biomass",
                 data,
                 time_column,
                 uniqueID,
                 boundaries,
                 coords = NULL,
                 crs = NULL,
                 ...){

  # TODO better CRS checks
  if (is.null(crs)){
    info_message <-
      paste0(" Warning: sspm is assuming that the CRS of boundaries is to be ",
             "used for casting")
    cli::cli_alert_warning(info_message)
    crs <- sf::st_crs(boundaries)
  }

  # 1. Ingest data and perform the correct checks
  the_spapspm_data <- as_sspm_data(data = data,
                                   time_column = time_column,
                                   coords = coords,
                                   name = name,
                                   uniqueID = uniqueID,
                                   crs = crs,
                                   ...)

  # 2. Create basis sspm object
  the_object <- new("sspm",
                    name = model_name,
                    data = the_spapspm_data,
                    boundaries = boundaries)

  return(the_object)
}


