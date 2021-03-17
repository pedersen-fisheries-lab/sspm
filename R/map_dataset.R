#' Map dataset onto a discretized sspm object
#'
#' After discretizing, the next step is to map coavriate datasets.
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'     [sspm][sspm-class] or [sspm_discrete][sspm_discrete-class]
#' @inheritParams as_sspm_data
#' @inheritDotParams as_sspm_data
#'
#' @return
#' The updated object, of class [sspm][sspm-class] or
#' [sspm_discrete][sspm_discrete-class].
#'
#' @export
setGeneric(name = "map_dataset",
           def = function(sspm_object,
                          data,
                          name,
                          time_column,
                          uniqueID,
                          coords = NULL,
                          crs = NULL,
                          ...){
             standardGeneric("map_dataset")
           }
)

# Methods -----------------------------------------------------------------

# Have to duplicate code here because setting class union between data.frame
# and sf is not possible as `sf` is not exported.

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm",
                    data = "data.frame"),
          function(sspm_object, data, name, time_column, uniqueID, coords, crs, ...){

            # TODO better CRS checks
            if (is.null(crs)){
              info_message <-
                paste0(" Warning: sspm is assuming that the CRS of boundaries is to be ",
                       "used for casting")
              cli::cli_alert_warning(info_message)
              crs <- sf::st_crs(spm_boundaries(sspm_object))
            }

            updated_object <-
              cast_and_return(sspm_object, data, name,
                              time_column, uniqueID, coords, crs, ...)

            return(updated_object)

          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm",
                    data = "sf"),
          function(sspm_object, data, name,
                   time_column, uniqueID, coords, crs, ...){

            updated_object <- cast_and_return(sspm_object, data, ...)

            return(updated_object)

          }
)

# Helper for the two methods above
cast_and_return <- function(sspm_object, data, name,
                            time_column, uniqueID, coords, crs, ...){

  # Cast data.frame as sspm_data
  sspm_data <- as_sspm_data(data = data, name,
                            time_column, uniqueID, coords, crs, ...)

  # Call next method
  updated_object <- map_dataset(sspm_object = sspm_object,
                                data = sspm_data)
  return(updated_object)
}

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm",
                    data = "sspm_data"),
          function(sspm_object, data, ...){

            # Append to list of mapped_datasets
            datasets <- spm_datasets(sspm_object)
            datasets_names <- names(datasets)

            if(sum(spm_name(data) %in% datasets_names)){

              cli::cli_alert_danger(" Name provided is already used, all dataset names must be unique.")

            }

            if (checkmate::test_class(sspm_object, "sspm_discrete")){

              data <- join_datasets(sspm_data = data,
                                    sspm_object = sspm_object)

            }

            datasets_tmp <- append(datasets, list(data))
            names(datasets_tmp) <- c(datasets_names, spm_name(data))

            spm_datasets(sspm_object) <- datasets_tmp

            # Return updated sspm_discretized
            return(sspm_object)
          }
)

#' @export
#' @describeIn map_dataset TODO
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm",
                    data = "list"),
          function(sspm_object, data,
                   name,
                   time_column,
                   uniqueID,
                   coords = NULL,
                   crs = NULL, ...){

            # Capture args
            args <- list(name = name,
                         time_colu = time_column,
                         uniqueID = uniqueID,
                         coords = coords)

            other_args <- list(...)
            args <- append(args, other_args)

            # Make checks on length of the name argument
            # must of the same length than data, and unique values
            if(length(args$name) != length(data) | length(unique(args$name)) != length(data)){
              stop(paste0("Argument 'name' is of length ", length(args$name),
                          ", but should be a unique set of the same length than data."),
                   call. = FALSE)
            }

            # Make checks on coords, either a character vector or a list
            if(test_class(args$coords, "character")){
              if(length(args$coords) == 2){
                args$coords <- list(args$coords)
              } else{
                stop("Argument 'coords' should be of a vector of length 2.",
                     call. = FALSE)
              }
            } else if (test_class(args$coords, "list")){
              if(length(args$coords) != 1) {
                if(length(args$coords) != length(data)){
                  stop("Argument 'coords' should be a list of the same length than data.",
                       call. = FALSE)
                }
              }
              if(any(sapply(args$coords, length) != 2)){
                stop("Argument 'coords' should be a list of elements of length 2.",
                     call. = FALSE)
              }
            }

            # Verify correct length of args
            for (arg_id in seq_len(length.out = length(args))){
              if(length(args[[arg_id]]) == length(data)){
                next
              } else if (length(args[[arg_id]]) == 1){
                args[[arg_id]] <- rep(args[[arg_id]], length(data))
              } else{
                stop(paste0("Argument '", names(args[arg_id]), "' is of length ",
                            length(args[[arg_id]]), ". Must be of length 1 or of the same length than data."),
                     call. = FALSE)
              }
            }

            tmp_sspm <- sspm_object
            for (dat_id in seq_len(length.out = length(data))){
              args_to_pass <- lapply(args, dplyr::nth, dat_id)
              args_to_pass$data <- data[[dat_id]]
              args_to_pass$sspm_object <- tmp_sspm
              tmp_sspm <- do.call(map_dataset, args = args_to_pass)
            }

            return(tmp_sspm)
          }
)
