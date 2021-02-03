#' Create a spaspm model object
#'
#' TODO
#'
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

# -------------------------------------------------------------------------

#' Create a spaspm_data structure
#'
#' TODO
#'
#' @export
setGeneric(name = "as_spaspm_data",
           def = function(data, coords, dataset_name, uniqueID, crs, ...){

             if(!checkmate::test_subset(uniqueID, names(data))){
               stop("`uniqueID` must be a column of `data`")
             }

             standardGeneric("as_spaspm_data")
           }
)

# Methods -----------------------------------------------------------------

#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "data.frame", coords = "NULL"),
          function(data, coords, dataset_name, uniqueID, crs, ...){

            stop("Argument `coords` must be provided when data matrix is a dataframe",
                 call. = FALSE)
          }
)

# If data.frame with coords, make it sf
#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "data.frame", coords = "character"),
          function(data, coords, dataset_name, uniqueID, crs, ...){

            # TODO CRS checks

            # Check coords
            if(!checkmate::test_subset(coords, names(data))){
              stop("`coords` must be columns of `data`")
            }

            # From a data.frame and coords, cast as sf (keep columns)
            info_message <-
              paste0("Casting data matrix into simple feature collection using columns: ",
                     paste(cli::col_green(coords), collapse = ", "))
            cli::cli_alert_info(info_message)

            new_data <- sf::st_as_sf(x = data, coords = coords, crs = crs,
                                     remove = FALSE)

            the_spaspm_data <- new("spaspm_data",
                                   name = dataset_name,
                                   data = new_data,
                                   uniqueID = uniqueID,
                                   coords = coords,
                                   representation = "Simple feature collection")

            return(the_spaspm_data)
          }
)

# If sf, ingest as is
#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "sf", coords = "ANY"),
          function(data, coords, uniqueID, crs, ...){

            # TODO CRS checks

            the_spaspm_data <- new("spaspm_data",
                                   name = dataset_name,
                                   data = data,
                                   uniqueID = uniqueID,
                                   coords = coords,
                                   representation = "Simple feature collection")

            return(the_spaspm_data)
          }
)
