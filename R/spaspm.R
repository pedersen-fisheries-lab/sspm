#' @import sf
#' @importFrom rlang .data
#' @importFrom methods new show

# Model Object Constructor
#' @export
spaspm <- function(data,
                   name = "My model",
                   coords = c("lat", "lon"),
                   uniqueID,
                   boundaries,
                   ...){

  if(!checkmate::test_subset(coords, names(data))){
    stop("`coords` must be columns of `data`")
  }

  the_spapspm_data <- as_spaspm_data(data, coords, uniqueID)

  the_object <- new("spaspm",
                    name = name,
                    uniqueID = uniqueID,
                    data = the_spapspm_data,
                    boundaries = boundaries)

  return(the_object)
}

# -------------------------------------------------------------------------

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
                                   coords = "No coordinates given",
                                   representation = "dataframe")

            return(the_spaspm_data)
          }
)

#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "data.frame", coords = "character"),
          function(data, coords, uniqueID, ...){

            # From a data.frame and coords, cast as sf
            the_data <- sf::st_as_sf(x = data, coords = coords)
            the_spaspm_data <- new("spaspm_data",
                                   data = data,
                                   uniqueID = uniqueID,
                                   is_spatial = TRUE,
                                   coords = coords,
                                   representation = "spatial dataframe")

            return(the_spaspm_data)
          }
)
