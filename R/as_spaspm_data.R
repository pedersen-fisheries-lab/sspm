#' Create a `spaspm_data` dataset structure
#'
#' This casts a `data.frame` or `sf` object into  an object of class
#' [`spaspm_data`][spaspm_data-class].
#'
#' @param data **\[data.frame OR sf\]** The dataset.
#' @param time_col **\[character\]** The column of `data` for the temporal
#'     dimensions (i.e. year).
#' @param coords **\[character\]** The column of `data` for longitude and
#'     latitude of the observations.
#' @param name **\[character\]** The name of the dataset, default to "Biomass".
#' @param uniqueID **\[character\]** The column of `data` that is unique for all
#'     rows of the data matrix.
#' @param crs Coordinate reference system, passed onto [st_as_sf][sf].
#'
#' @return
#' An object of class [`spaspm_data`][spaspm_data-class].
#'
#' @export
setGeneric(name = "as_spaspm_data",
           def = function(data, time_col, coords, name, uniqueID, crs){

             if(!checkmate::test_subset(uniqueID, names(data))){
               stop("`uniqueID` must be a column of `data`", call. = FALSE)
             }
             if(!(length(unique(data[[uniqueID]])) == nrow(data))){
               stop("`uniqueID` must be unique for each row of `data`", call. = FALSE)
             }

             if(!checkmate::test_subset(time_col, names(data))){
               stop("`time_col` must be a column of `data`", call. = FALSE)
             }

             standardGeneric("as_spaspm_data")
           }
)

# Methods -----------------------------------------------------------------

#' @describeIn as_spaspm_data TODO
#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "data.frame", coords = "missingOrNULL"),
          function(data, time_col, coords, name, uniqueID, crs){

            stop("Argument `coords` must be provided when data matrix is a dataframe",
                 call. = FALSE)
          }
)

# If data.frame with coords, make it sf
#' @describeIn as_spaspm_data TODO
#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "data.frame", coords = "character"),
          function(data, time_col, coords, name, uniqueID, crs){

            # TODO CRS checks

            # Check coords
            if(!checkmate::test_subset(coords, names(data))){
              stop("`coords` must be columns of `data`", call. = FALSE)
            }

            # From a data.frame and coords, cast as sf (keep columns)
            info_message <-
              paste0(" Casting data matrix into simple feature collection using columns: ",
                     paste(cli::col_green(coords), collapse = ", "))
            cli::cli_alert_info(info_message)

            new_data <- sf::st_as_sf(x = data, coords = coords, crs = crs,
                                     remove = FALSE)

            the_spaspm_data <- new("spaspm_data",
                                   name = name,
                                   data = new_data,
                                   time_col = time_col,
                                   uniqueID = uniqueID,
                                   coords = coords,
                                   representation = "Simple feature collection")

            return(the_spaspm_data)
          }
)

# If sf, ingest as is
#' @describeIn as_spaspm_data TODO
#' @export
setMethod(f = "as_spaspm_data",
          signature(data = "sf", coords = "ANY"),
          function(data, time_col, coords, name, uniqueID, crs){

            # TODO CRS checks

            the_spaspm_data <- new("spaspm_data",
                                   name = name,
                                   data = data,
                                   time_col = time_col,
                                   uniqueID = uniqueID,
                                   coords = coords,
                                   representation = "Simple feature collection")

            return(the_spaspm_data)
          }
)
