#' Map dataset onto a discretized sspm object
#'
#' After discretizing, the next step is to map coavriate datasets.
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'     [sspm][sspm-class] or [sspm_discrete][sspm_discrete-class].
#' @inheritParams as_sspm_data
#' @inheritDotParams as_sspm_data
#' @param catch_column **\[character\]** For mapping catch data only: the name
#'     of the column containing catch data in the catch dataset.
#' @param biomass_column **\[character\]** For mapping catch data only: the name
#'     of the column for which the catch has to be pondered with.
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
                          type,
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
#' @rdname map_dataset
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm",
                    data = "data.frame"),
          function(sspm_object, data, name, type, time_column, uniqueID, coords, crs, ...){

            # TODO better CRS checks
            if (is.null(crs)){
              info_message <-
                paste0(" Warning: sspm is assuming that the CRS of boundaries is to be ",
                       "used for casting")
              cli::cli_alert_warning(info_message)
              crs <- sf::st_crs(spm_boundaries(sspm_object))
            }

            updated_object <-
              cast_and_return(sspm_object, data, name, type,
                              time_column, uniqueID, coords, crs, ...)

            return(updated_object)

          }
)

#' @export
#' @rdname map_dataset
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm",
                    data = "sf"),
          function(sspm_object, data, name, type,
                   time_column, uniqueID, coords, crs, ...){

            updated_object <- cast_and_return(sspm_object, data, ...)

            return(updated_object)

          }
)

# Helper for the two methods above
cast_and_return <- function(sspm_object, data, name, type,
                            time_column, uniqueID, coords, crs, ...){

  # Cast data.frame as sspm_data
  sspm_data <- as_sspm_data(data = data, name, type,
                            time_column, uniqueID, coords, crs, ...)

  # Call next method
  updated_object <- map_dataset(sspm_object = sspm_object,
                                data = sspm_data)
  return(updated_object)
}

#' @export
#' @rdname map_dataset
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm",
                    data = "sspm_data"),
          function(sspm_object, data, ...){

            # Append to list of mapped_datasets
            datasets <- spm_datasets(sspm_object)
            datasets_names <- names(datasets)

            # Verity the number of types is respectec
            all_types <- sapply(datasets, spm_type)
            this_type <- spm_type(data)

            # TODO consider not exporting map_dataset on use only current shortcuts
            # to avoid replicated code
            if(this_type == "catch"){
              check_sspm_for_catch(sspm_object)
            }

            if(this_type %in% c("biomass", "catch")){
              if(this_type %in% all_types){
                cli_alert_danger(paste0("A dataset of type ", this_type,
                                        " is already mapped, but only one dataset of this type is allowed."))
                stop("Only one dataset of this type is allowed", call. = FALSE)
              }
            }

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
#' @rdname map_dataset
setMethod(f = "map_dataset",
          signature(sspm_object = "sspm",
                    data = "list"),
          function(sspm_object,
                   data,
                   name,
                   type,
                   time_column,
                   uniqueID,
                   coords = NULL,
                   crs = NULL, ...){

            # Capture args
            args <- list(name = name,
                         type = type,
                         time_column = time_column,
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
            if(checkmate::test_class(args$coords, "character")){
              if(length(args$coords) == 2){
                args$coords <- list(args$coords)
              } else{
                stop("Argument 'coords' should be of a vector of length 2.",
                     call. = FALSE)
              }
            } else if (checkmate::test_class(args$coords, "list")){
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

# Topical methods ---------------------------------------------------------

# Map_biomass and predictor -----------------------------------------------

#' @export
#' @rdname map_dataset
setGeneric(name = "map_biomass",
           def = function(sspm_object,
                          data,
                          name,
                          type = "biomass",
                          time_column,
                          uniqueID,
                          coords = NULL,
                          crs = NULL,
                          ...){
             standardGeneric("map_biomass")
           }
)

#' @export
#' @rdname map_dataset
setMethod(f = "map_biomass",
          signature(sspm_object = "sspm",
                    data = "data.frame"),
          function(sspm_object, data, name, type, time_column, uniqueID, coords, crs, ...){
            map_dataset(sspm_object, data, name, type, time_column, uniqueID, coords, crs, ...)
          }
)

#' @export
#' @rdname map_dataset
setGeneric(name = "map_predictor",
           def = function(sspm_object,
                          data,
                          name,
                          type = "predictor",
                          time_column,
                          uniqueID,
                          coords = NULL,
                          crs = NULL,
                          ...){
             standardGeneric("map_predictor")
           }
)

#' @export
#' @rdname map_dataset
setMethod(f = "map_predictor",
          signature(sspm_object = "sspm",
                    data = "data.frame"),
          function(sspm_object, data, name, type, time_column, uniqueID, coords, crs, ...){
            map_dataset(sspm_object, data, name, type, time_column, uniqueID, coords, crs, ...)
          }
)

# Map_catch ---------------------------------------------------------------

#' @export
#' @rdname map_dataset
setGeneric(name = "map_catch",
           def = function(sspm_object,
                          data,
                          name,
                          type = "catch",
                          time_column,
                          uniqueID,
                          coords = NULL,
                          crs = NULL,
                          catch_column,
                          biomass_column,
                          ...){
             standardGeneric("map_catch")
           }
)

#' @export
#' @rdname map_dataset
setMethod(f = "map_catch",
          signature(sspm_object = "sspm",
                    data = "data.frame"),
          function(sspm_object, data, name, type, time_column, uniqueID, coords,
                   crs, catch_column, biomass_column, ...){

            check_sspm_for_catch(sspm_object)

            sspm_object <-
              map_dataset(sspm_object, data, name, type, time_column, uniqueID, coords, crs, ...)

            # Aggregate to the polygon level
            catch_dataset <- spm_datasets(sspm_object, "catch")
            time_col <- spm_time_column(catch_dataset)

            catch_data <- spm_data(catch_dataset) %>%
              dplyr::group_by(.data[[time_col]], patch_id) %>%
              sf::st_drop_geometry() %>%
              dplyr::summarise(total_catch = sum(.data[[catch_column]],
                                                 na.rm = TRUE)) %>%
              tidyr::complete(.data[[time_col]], patch_id,
                              fill = list(total_catch = 0)) %>%
              dplyr::mutate(!!time_col :=
                              as.factor(.data[[time_col]])) %>%
              unique()

            # Calculate the right columns
            smooth_dataset <- spm_smoothed_data(sspm_object)
            smoothed_data <- spm_data(smooth_dataset)
            smoothed_data_time_col <- spm_time_column(smooth_dataset)

            # First, join data
            smoothed_data_mod <- smoothed_data %>%
              dplyr::mutate(!!smoothed_data_time_col :=
                              as.factor(.data[[smoothed_data_time_col]])) %>%
              dplyr::rename(!!time_col := smoothed_data_time_col) %>%
              dplyr::left_join(catch_data,
                               by = sapply(c(time_col, "patch_id"),
                                           rlang::as_string))

            smooth_data_with_new_cols <- smoothed_data_mod %>%
              dplyr::mutate(
                "{biomass_column}_with_catch" :=
                  borealis_smooth + total_catch/area_km2) %>%
              dplyr::mutate(
                "{biomass_column}_with_catch_change" :=
                  log(borealis_smooth_with_catch) - log(lag(borealis_smooth))
              ) %>%
              dplyr::relocate(dplyr::starts_with(biomass_column), .after = row_ID)

            spm_data(spm_smoothed_data(sspm_object)) <- smooth_data_with_new_cols

            return(sspm_object)

          }
)

# Helpers -----------------------------------------------------------------

check_sspm_for_catch <- function(sspm_object){

  # Map only if discrete already

  if(!checkmate::test_class(sspm_object, "sspm_discrete")){
    cli::cli_alert_danger(" Catch data can only be mapped once the model has been discretized")
    stop("sspm object is not discrete", call. = FALSE)
  }

  # Map catch only maps onto smoothed data at the sspm level and lead to
  # the calculation of balanced catch for the biomass data.

  if(is.null(spm_data(spm_smoothed_data(sspm_object)))){
    cli::cli_alert_danger(" Catch data can only be mapped once the model has been smoothed")
    stop("sspm object must be smoothed", call. = FALSE)
  }

}

