#' Create a `sspm` model object
#'
#' Create a sspm_model object.
#'
#' @param biomass **\[sspm_dataset (smoothed)\]** The dataset containing the
#'     biomass variable.
#' @param predictors **\[list  OF sspm_dataset (smoothed)\]** The list of predictor
#'     datasets.
#' @param catch **\[sspm_dataset (smoothed)\]** Optional, the catch data
#' @param biomass_var **\[character\]** The biomass column of `biomass`.
#' @param catch_var **\[character\]** The catch column of `catch`.
#'
#' @return
#' An object of class  [sspm][sspm-class].
#'
#' @rdname sspm-constructor
#' @export
setGeneric(name = "sspm",
           def = function(biomass,
                          predictors,
                          catch = NULL,
                          biomass_var = NULL,
                          catch_var = NULL) {
             standardGeneric("sspm")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(biomass = "sspm_dataset",
                    predictors = "sspm_dataset"),
          function(biomass, predictors, catch, biomass_var, catch_var) {

            # Turn predictors to list
            predictors <- list(predictors)
            names(predictors) <- sapply(predictors, spm_name)
            sspm(biomass, predictors, catch, biomass_var, catch_var)

          }
)

#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(biomass = "sspm_dataset",
                    predictors = "list"),
          function(biomass, predictors, catch, biomass_var, catch_var) {

            # 1. Check all predictors in list are sspm_dataset
            if (any(!is_sspm_dataset(predictors))) {
              cli::cli_alert_danger("Some predictors are not of class sspm_dataset")

            } else {

              # 2. Check boundaries
              biomass_boundaries <- spm_boundaries(biomass)
              predictors_boundaries <- lapply(predictors, spm_boundaries)
              all_boundaries <- unname(append(list(biomass_boundaries),
                                              predictors_boundaries))

              if (!check_identical_boundaries(all_boundaries)) {
                cli::cli_alert_danger("not all datasets have the same boundaries object")
                stop(call. = FALSE)
              }

              # 3. combine the full_smoothed_data
              # message about catch
              info_message <-
                paste0(" Joining smoothed data from all datasets")
              cli::cli_alert_info(info_message)

              biomass_clean <- clean_data_for_joining(spm_smoothed_data(biomass))
              joining_vars <- c("patch_id", spm_boundary_colum(spm_boundaries(biomass)))
              if ("area_km2" %in% names(biomass_clean)) {
                joining_vars <- c(joining_vars, "area_km2")
              }

              full_smoothed_data <- biomass_clean
              for (predictor in predictors) {

                the_suffix <- c(paste0("_", spm_name(biomass)),
                                paste0("_", spm_name(predictor)))

                dataset <- predictor %>%
                  spm_smoothed_data() %>%
                  clean_data_for_joining() %>%
                  dplyr::rename(!!spm_time_column(biomass) :=
                                  spm_time_column(predictor))

                full_smoothed_data <- full_smoothed_data %>%
                  dplyr::left_join(dataset,
                                   by = c(dplyr::all_of(joining_vars),
                                          spm_time_column(biomass)),
                                   suffix = the_suffix)

              }

              full_smoothed_data <- full_smoothed_data %>%
                dplyr::left_join(dplyr::select(spm_patches(spm_boundaries(biomass)),
                                               c("patch_id")),
                                 by = "patch_id") %>%
                sf::st_as_sf() %>%
                tibble::rowid_to_column("row_ID")

              if (!is.null(catch)) {

                # 4. deal with catch data

                if (any(sapply(list(biomass_var, catch_var), is.null))) {

                  cli::cli_alert_danger("biomass_var or catch_var missing")
                  stop(call. = FALSE)

                } else {
                  matches <- sum(grepl(biomass_var, colnames(full_smoothed_data)))
                  if (matches > 1) {
                    cli::cli_alert_warning("More than one columns matching the biomass_var variable")
                    cli::cli_alert_warning(paste0("You might need to set biomass_var to ",
                                                  cli::col_yellow(paste0(biomass_var, "_", spm_name(biomass)))))
                  } else {
                    # message about catch
                    info_message <-
                      paste0(" Offsetting biomass with catch data using columns: ",
                             paste(cli::col_green(c(biomass_var, catch_var)), collapse = ", "))
                    cli::cli_alert_info(info_message)
                  }
                }

                if (!is_mapped(catch)) {
                  # Need to map dataset
                  catch <- join_datasets(catch, spm_boundaries(biomass))
                }

                time_col <- spm_time_column(catch)
                catch_data <- spm_data(catch) %>%
                  dplyr::group_by(.data[[time_col]], .data$patch_id) %>%
                  sf::st_set_geometry(NULL) %>%
                  dplyr::summarise(total_catch = sum(.data[[catch_var]],
                                                     na.rm = TRUE)) %>%
                  tidyr::complete(.data[[time_col]], .data$patch_id,
                                  fill = list(total_catch = NA)) %>%
                  dplyr::mutate(!!time_col :=
                                  as.factor(.data[[time_col]])) %>%
                  unique()

                # Calculate the right columns
                smoothed_data_time_col <- spm_time_column(biomass)

                # First, join data
                full_smoothed_data <- full_smoothed_data %>%
                  dplyr::mutate(!!smoothed_data_time_col :=
                                  as.factor(.data[[smoothed_data_time_col]])) %>%
                  dplyr::rename(!!time_col := smoothed_data_time_col) %>%
                  dplyr::left_join(catch_data,
                                   by = sapply(c(time_col, "patch_id"),
                                               rlang::as_string))

                catch_name <- paste0(biomass_var, "_with_catch")
                change_name <- paste0(biomass_var, "_with_catch")

                full_smoothed_data <- full_smoothed_data %>%
                  dplyr::group_by(.data[["patch_id"]],
                                  !!spm_boundary_colum(biomass_boundaries)) %>%
                  dplyr::mutate(
                    !!catch_name :=
                      .data[[biomass_var]] + .data$total_catch / .data$area_km2) %>%
                  dplyr::mutate(
                    !!change_name :=
                      log(.data[[catch_name]]) - log(dplyr::lag(.data[[biomass_var]],
                                                                default = NA))) %>%
                  dplyr::ungroup() %>%
                  dplyr::mutate(!!change_name := ifelse(is.na(.data[[change_name]]),
                                                        0, .data[[change_name]])) %>%
                  dplyr::relocate(dplyr::starts_with(biomass_var),
                                  .after = .data$row_ID)

                # 3. create and return object
                biomass_name <- spm_name(biomass)
                all_data <- append(list(biomass = biomass,
                                        catch = catch),
                                   predictors)
                names(all_data)[1] <- biomass_name

              } else {

                # 3. create and return object
                biomass_name <- spm_name(biomass)
                all_data <- append(list(biomass = biomass),
                                   predictors)
                names(all_data)[1] <- biomass_name

              }

              new_sspm <- new("sspm",
                              datasets = all_data,
                              time_column = spm_time_column(biomass),
                              uniqueID = "row_ID",
                              boundaries = spm_boundaries(biomass),
                              smoothed_data = full_smoothed_data,
                              is_split = FALSE)
            }

          }
)


# Check if boundaries are identical
check_identical_boundaries <- function(boundaries) {
  do.call(identical, boundaries)
}

# Check whether of sspm_dataset class of a list of objects
is_sspm_dataset <- function(list_of_datasets) {
  checkmate::assert_list(list_of_datasets)
  checks <- sapply(list_of_datasets,
                   function(x) {checkmate::test_class(x, "sspm_dataset")})
  return(checks)
}

# clean up data frame before we can join
clean_data_for_joining <- function(dataset) {
  dataset %>% dplyr::select(-.data$row_ID) %>% sf::st_drop_geometry()
}
