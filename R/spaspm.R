#' Create a spaspm model object
#'
#' TODO
#'
#' @export
spaspm <- function(data,
                   name = "My SPASPM model",
                   coords = c('lon','lat'),
                   uniqueID,
                   boundaries,
                   ...){

  # 1. Ingest data and perform the correct checks
  the_spapspm_data <- as_spaspm_data(data, coords, uniqueID)

  # 2. Create basis spaspm object
  the_object <- new("spaspm",
                    name = name,
                    data = the_spapspm_data,
                    boundaries = boundaries)

  return(the_object)
}

# Methods -----------------------------------------------------------------

#' @export
setGeneric(name = "as_spaspm_data",
           def = function(data, coords, uniqueID, ...){

             if(!checkmate::test_subset(uniqueID, names(data))){
               stop("`uniqueID` must be a column of `data`")
             }

             standardGeneric("as_spaspm_data")
           }
)

#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "data.frame", coords = "NULL"),
          function(data, coords, uniqueID, ...){

            the_spaspm_data <- new("spaspm_data",
                                   uniqueID = uniqueID,
                                   is_spatial = FALSE,
                                   coords = NULL,
                                   representation = "dataframe")

            return(the_spaspm_data)
          }
)

# If data.frame with coords, make it sf
#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "data.frame", coords = "character"),
          function(data, coords, uniqueID, ...){

            # Check coords
            if(!checkmate::test_subset(coords, names(data))){
              stop("`coords` must be columns of `data`")
            }

            # From a data.frame and coords, cast as sf
            the_data <- sf::st_as_sf(x = data, coords = coords)
            the_spaspm_data <- new("spaspm_data",
                                   data = data,
                                   uniqueID = uniqueID,
                                   is_spatial = TRUE,
                                   coords = coords,
                                   representation = "Simple feature collection")

            return(the_spaspm_data)
          }
)

# If sf, ingest as is
#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "sf", coords = "ANY"),
          function(data, coords, uniqueID, ...){

            the_spaspm_data <- new("spaspm_data",
                                   data = data,
                                   uniqueID = uniqueID,
                                   is_spatial = TRUE,
                                   coords = NULL,
                                   representation = "Simple feature collection")

            return(the_spaspm_data)
          }
)
