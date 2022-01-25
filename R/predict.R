#' Predict with a SPM model
#'
#' Predict using a fitted SPM model on the whole data or on new data
#'
#' @param object *\[sspm_fit\]** Fit object to predict from.
#' @param new_data **\[data.frame\]** New data to predict with.
#' @param biomass **\[character\]** Biomass variable.
#' @param aggregate **\[logical\]** For biomass predictions only, whether to
#'    aggregate the data to the boundary level. Default to FALSE.
#' @param interval **\[logical\]** Whether or not to calculate confidence, and
#'    when possible, prediction intervals.
#' @param next_ts **\[logical\]** For biomass, predict next timestep.
#'
#' @return
#' A `dataframe` of predictions.
#'
#' @export
#' @name predict
#' @aliases predict.sspm

NULL

#' @rdname predict
setMethod(f = "predict",
          signature(object = "sspm_fit"),
          function(object, new_data = NULL, biomass = NULL, aggregate = FALSE,
                   interval = FALSE, next_ts = FALSE, type = "response") {

            # Get data
            bounds <- spm_boundaries(object)
            bounds_col <- spm_boundary_column(bounds)
            patch_area_col <- spm_patches_area_column(bounds)
            time_col <- spm_time_column(object)
            patches <- spm_patches(bounds)
            patch_area_col <- spm_patches_area_column(bounds)

            # If biomass variable is not provided, we are predicting productivity
            if(is.null(biomass)){

              # If no new data, take the smoothed data and the vars from the
              # formula. In both cases, turn into a list
              if (is.null(new_data)){

                new_data <- append(as.list(spm_smoothed_data(object)),
                                   formula_vars(spm_formulas(object)))

              } else {

                # Otherwise, make some checks
                checkmate::assert_class(new_data, "data.frame")
                new_data <- as.list(new_data)

              }

              # Retrieve the fit
              object_fit <- spm_get_fit(object)

              # Predict with mgcv
              pred_log <- object_fit %>%
                mgcv::predict.bam(newdata = new_data, type = type)

              # Make a df with the log and non log version of those predictions
              preds_df <- data.frame(pred_log = pred_log) %>%
                dplyr::mutate(pred = exp(pred_log))

              # If we want to compute the intervals, do so with the helpers
              if (interval) {

                # Compute simulations
                sims <- produce_sims(object_fit, new_data)

                # Confidence interval
                CI <- confidence_interval(sims)

                # Prediction interval
                PI <- prediction_interval(object_fit, sims)

                # Bind all
                preds_df <- dplyr::bind_cols(preds_df, CI, PI)

              }

              # Keep the minimum column set
              columns_to_keep <- spm_smoothed_data(object) %>%
                dplyr::select(.data$patch_id, !!time_col,
                              !!bounds_col, !!patch_area_col)

              # Bind and turn into sf object
              preds_df <- cbind(preds_df, columns_to_keep)  %>%
                sf::st_as_sf() # TODO verify CRS

            } else { # Else we are predicting biomass

              # Verify that the biomass column character is present in the data
              checkmate::assert_class(biomass, "character")
              data <- spm_smoothed_data(object)
              if (!checkmate::test_subset(biomass, names(data))) {
                stop("`biomass` must be a column of `data`", call. = FALSE)
              }

              # Compute predictions for next timnestep if desired
              if (next_ts){

                # Use helpers to get next is data
                next_ts_data <- make_next_ts_data(object, time_col, patches,
                                                  bounds_col)

                # Apply the LINPRED in case it is needed
                linpred_lag_vars <- spm_formulas(object)@lag_vars

                if (!is.null(linpred_lag_vars)){
                  linpred <- LINPRED(next_ts_data$data_filtered,
                                     spm_boundaries(object),
                                     time_col, linpred_lag_vars,
                                     k = 5, m = 1)
                  mats <- linpred$vars

                  # Modify the mats to fit the time step under scrutiny
                  by_mat_nrow <- dim(mats$by_matrix)[1]
                  grid_length <- nrow(spm_patches(spm_boundaries(object)))
                  mats$lag_matrix <- mats$lag_matrix[1:grid_length,]
                  mats$by_matrix <- mats$by_matrix[(by_mat_nrow-grid_length+1):by_mat_nrow,]

                  # Create new data
                  new_data <- next_ts_data$new_data

                } else {
                  mats <- NULL
                }

                # Append the data
                new_data <- append(as.list(new_data), mats)

                # Calculate ratio, then density, and format the output
                ratio_next_ts <-
                  exp(mgcv::predict.bam(spm_get_fit(object), new_data))

                density_last_year <- spm_smoothed_data(object) %>%
                  dplyr::filter(.data$year_f %in% next_ts_data$max_ts) %>%
                  dplyr::pull(.data[[biomass]])

                preds_df <- patches %>%
                  dplyr::mutate(density_next_ts = density_last_year * ratio_next_ts,
                                biomass = .data$density_next_ts *
                                  (as.numeric(units::set_units(.data[[patch_area_col]],
                                                               value = "km^2"))),
                                year_f = next_ts_data$next_ts) %>%
                  dplyr::select(-.data$density_next_ts)

                # Cosmetic changes
                preds_df <-  preds_df %>%
                  dplyr::relocate(.data$biomass) %>%
                  dplyr::relocate(.data[[bounds_col]]) %>%
                  dplyr::relocate(.data[[time_col]])

              } else {

                biomass <- data[[biomass]]

                preds <- predict(object)

                biomass_density_with_catch <- preds$pred * biomass
                catch_density <- spm_smoothed_data(object)$catch_density
                biomass_pred <- biomass_density_with_catch - catch_density

                pred_subset <- dplyr::select(preds, -.data$pred, -.data$pred_log) %>%
                  dplyr::mutate(!!patch_area_col :=
                                  units::set_units(.data[[patch_area_col]],
                                                   value = "km^2")) %>%
                  dplyr::relocate(.data[[patch_area_col]], .before = "geometry")

                preds_df <- pred_subset %>%
                  cbind(data.frame(biomass_density_with_catch = biomass_density_with_catch,
                                   biomass_density =  biomass_pred)) %>%
                  dplyr::mutate(biomass_with_catch = .data$biomass_density_with_catch *
                                  as.numeric(.data[[patch_area_col]]),
                                biomass = .data$biomass_density *
                                  as.numeric(.data[[patch_area_col]])) %>%
                  dplyr::relocate(.data$biomass_with_catch, .data$biomass,
                                  .before = "geometry") %>%
                  sf::st_as_sf() # TODO check CRS

              }

              if (aggregate) {

                preds_df <- preds_df %>%
                  dplyr::group_by(.data[[bounds_col]], .data[[time_col]]) %>%
                  dplyr::summarise(biomass = sum(biomass))

              }



            }

            return(preds_df)
          }
)

#' @rdname predict
setMethod(f = "predict",
          signature(object = "sspm_dataset"),
          function(object, new_data = NULL, discrete = TRUE, type = "response") {

            time_col <- spm_time_column(object)
            the_fit <- spm_smoothed_fit(object)
            the_formulas <- spm_formulas(object)
            responses <- sapply(the_formulas, spm_response)

            if (is.null(spm_smoothed_fit(object))) {
              stop("fit is missing, refit model with keep_fit = TRUE")
            }

            if (!is.null(new_data)){

              checkmate::assert_class(new_data, "data.frame")
              new_data <- as.list(new_data)

            } else {
              if (discrete) {

                new_data <-
                  make_prediction_matrix(the_data = spm_data(object),
                                         time_col = spm_time_column(object),
                                         patches = spm_patches(spm_boundaries(object)))

              } else {

                new_data <- spm_data(object)

              }

            }

            preds <- as.data.frame(lapply(the_fit, mgcv::predict.bam,
                                          newdata = new_data,
                                          type = type))
            names(preds) <- responses

            biomass_vars <- c(object@biomass)
            biomass_density_vars <- c(object@density)

            for (var in names(preds)){
              if (var %in% biomass_vars){
                preds[[var]] <- set_biomass(preds[[var]])
              } else if (var %in% biomass_density_vars){
                preds[[var]] <- set_biomass_density(preds[[var]])
              }
            }

            if (discrete) {
              preds <- preds %>%
                dplyr::bind_cols(new_data)
            }

            return(preds)
          }
)

# Helpers -----------------------------------------------------------------

# Build the new_data for the next timestep predictions
make_next_ts_data <- function(object, time_col, patches, bounds_col,
                              year_lag = 5){

  # Check if all lagged var
  lagged_var_names <- get_lagged_var_names(object)

  # Get ts data
  all_ts <- as.numeric(as.character(
    unique(spm_smoothed_data(object)[[time_col]])))
  max_ts <- max(all_ts)
  next_ts <- max_ts + 1

  # Make a new grid with the next timestep
  new_grid <- patches %>%
    dplyr::select(.data[[bounds_col]], .data$patch_id) %>%
    tidyr::expand_grid(!!time_col := next_ts)

  # Bind the new grid to the smooth data
  spm_smoothed_data(object) <- spm_smoothed_data(object) %>%
    dplyr::bind_rows(new_grid)

  # Lag the vars that need lagging (even if already present,
  # it is needed to account for the new year).
  object <- object %>% spm_lag(lagged_var_names, 1, default = NA)

  # Create the new data
  new_data <- spm_smoothed_data(object) %>%
    dplyr::filter(.data[[time_col]] == next_ts) %>%
    st_drop_geometry()

  # Filter to the past 5 years
  data_filtered <- spm_smoothed_data(object) %>%
    dplyr::filter(.data[[time_col]] %in% (next_ts-year_lag):next_ts)

  return(list(new_data = new_data,
              data_filtered = data_filtered,
              max_ts = max_ts,
              next_ts = next_ts))
}

# Obtain var names for predicting the model, excluding penalty mats
get_var_names <- function(sspm_object, exclude_mats = TRUE,
                          exclude_special = TRUE) {
  var_names <- sspm_object@fit$var.summary %>%
    names()
  if (exclude_special){
    var_names <- var_names %>%
      stringr::str_subset("matrix", negate = TRUE)
  }
  if (exclude_mats){
    var_names <- var_names %>%
      stringr::str_subset("patch_id", negate = TRUE) %>%
      stringr::str_subset(spm_time_column(sspm_object), negate = TRUE) %>%
      stringr::str_subset(spm_boundary_column(sspm_object), negate = TRUE)
  }
  return(var_names)
}

# Get the vars that are lagged
get_lagged_var_names <- function(sspm_object){

  all_var_names <- get_var_names(sspm_object, exclude_special = TRUE)
  lagged_var_names <- all_var_names %>%
    stringr::str_subset("_lag_1")

  if(!all(all_var_names %in% lagged_var_names)){
    vars_to_print <- all_var_names[!(all_var_names %in% lagged_var_names)]
    cli::cli_alert_warning(paste0("Not all vars are lagged vars: ",
                                  paste0(c(vars_to_print), collapse = ", ")))
  }

  lagged_var_names <- lagged_var_names %>%
    stringr::str_remove("_lag_1")

  return(lagged_var_names)
}

# Prepare the prediction matrix
make_prediction_matrix <- function(the_data, time_col, patches){

  year_vector <- as.numeric(as.character(the_data[[time_col]]))
  year_values <- sort(unique(year_vector))

  predict_mat <- patches %>%
    sf::st_set_geometry(NULL) %>%
    tidyr::expand_grid(!!time_col := year_values)

  return(predict_mat)
}
