#' Create a `sspm` model object
#'
#' Create a sspm_model object.
#'
#' @param biomass **\[sspm_dataset (smoothed)\]** The dataset containing the
#'     biomass variable.
#' @param predictors **\[list  OF sspm_dataset (smoothed)\]** The list of predictor
#'     datasets.
#'
#' @return
#' An object of class  [sspm][sspm-class].
#'
#' @rdname sspm-constructor
#' @export
setGeneric(name = "sspm",
           def = function(biomass,
                          predictors) {
             standardGeneric("sspm")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(biomass = "sspm_dataset",
                    predictors = "missing"),
          function(biomass, predictors) {

            new_sspm <- new("sspm",
                            datasets = list(biomass = biomass),
                            time_column = spm_time_column(biomass),
                            uniqueID = "row_ID",
                            boundaries = spm_boundaries(biomass),
                            smoothed_data = spm_smoothed_data(biomass),
                            is_split = FALSE)

          }
)


#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(biomass = "sspm_dataset",
                    predictors = "sspm_dataset"),
          function(biomass, predictors) {

            # Turn predictors to list
            predictors <- list(predictors)
            names(predictors) <- sapply(predictors, spm_name)
            sspm(biomass, predictors)

          }
)

#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(biomass = "sspm_dataset",
                    predictors = "list"),
          function(biomass, predictors) {

            # 1. Check all predictors in list are sspm_dataset
            if (any(!is_sspm_dataset(predictors))) {
              cli::cli_alert_danger("Some predictors are not of class sspm_dataset")

            } else {

              # 2. Check boundaries
              biomass_boundaries <- spm_boundaries(biomass)
              predictors_boundaries <- lapply(predictors, spm_boundaries)
              all_boundaries <- unname(append(list(biomass_boundaries),
                                              predictors_boundaries))
              check_identical_boundaries(all_boundaries)

              # 3. combine the full_smoothed_data
              # message about catch
              info_message <-
                paste0(" Joining smoothed data from all datasets")
              cli::cli_alert_info(info_message)

              biomass_clean <- clean_data_for_joining(spm_smoothed_data(biomass))
              joining_vars <- c("patch_id", spm_boundary_column(spm_boundaries(biomass)))
              if ("area" %in% names(biomass_clean)) {
                joining_vars <- c(joining_vars, "area")
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

              # 3. create and return object
              biomass_name <- spm_name(biomass)
              all_data <- append(list(biomass = biomass),
                                 predictors)
              names(all_data)[1] <- biomass_name

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
  boundaries <- lapply(boundaries, spm_boundaries)

  for (bound in seq_len(length(boundaries) - 1)){

    bound_check <- identical(boundaries[bound],
                             boundaries[bound + 1])

    if(!bound_check){
      cli::cli_alert_danger("not all datasets have the same boundaries object")
      stop(call. = FALSE)
    }

  }

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
