#' Predict with a SPM model
#'
#' Predict using a fitted SPM model on the whole data or on new data
#'
#' @param object *\[sspm_fit\]** Fit object to predict from.
#' @param new_data **\[data.frame\]** New data to predict with.
#' @param biomass **\[character\]** Biomass variable.
#' @param next_ts **\[logical\]** For biomass, predict next timestep.
#' @param ... Arguments passed on to [predict.bam].
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
          function(object, new_data = NULL, biomass = NULL,
                   next_ts = FALSE, ...) {

            if(is.null(biomass)){

              if (is.null(new_data)){

                new_data <- append(as.list(spm_smoothed_data(object)),
                                   formula_vars(spm_formulas(object)))

              } else {

                checkmate::assert_class(new_data, "data.frame")

                new_data <- as.list(new_data)

              }

              pred_log <- spm_get_fit(object) %>%
                predict.bam(newdata = new_data, type = "response", ...)

              preds_df <- data.frame(pred_log = pred_log) %>%
                dplyr::mutate(pred = exp(pred_log))

              columns_to_keep <- spm_smoothed_data(object) %>%
                dplyr::select(.data$patch_id, !!spm_time_column(object),
                              !!spm_boundary_column(spm_boundaries(object)))

              preds_df <- cbind(preds_df, columns_to_keep)  %>%
                sf::st_as_sf()

            } else {

              data <- spm_smoothed_data(object)

              if (!checkmate::test_subset(biomass, names(data))) {
                stop("`biomass` must be a column of `data`", call. = FALSE)
              }

              if (checkmate::test_class(biomass, "character")){

                if (next_ts){

                  # Collect the needed info
                  bounds <- spm_boundaries(object)
                  bounds_col <- spm_boundary_column(bounds)
                  time_col <- spm_time_column(object)
                  patches <- spm_patches(bounds)

                  # Check if all lagged var
                  lagged_var_names <- get_lagged_var_names(object)

                  all_ts <- as.numeric(as.character(
                    unique(spm_smoothed_data(object)[[time_col]])))
                  max_ts <- max(all_ts)
                  next_ts <- max_ts + 1

                  # Make a new grid with the next timestep
                  new_grid <- patches %>%
                    dplyr::select(.data[[bounds_col]], .data$patch_id) %>%
                    tidyr::expand_grid(!!time_col := next_ts)
                  grid_length <- nrow(new_grid)

                  # Bind the new grid to the smooth data
                  spm_smoothed_data(object) <- spm_smoothed_data(object) %>%
                    dplyr::bind_rows(new_grid)

                  # Lag the vars that need lagging (even if already present,
                  # it is needed for the new year).
                  object <- object %>%
                    spm_lag(lagged_var_names, 1, default = NA)

                  # Create the new data
                  new_data <- spm_smoothed_data(object) %>%
                    dplyr::filter(.data[[time_col]] == next_ts) %>%
                    st_drop_geometry()

                  # Filter to the past 5 years
                  data_filtered <- spm_smoothed_data(object) %>%
                    dplyr::filter(.data[[time_col]] %in% (next_ts-5):next_ts)

                  # Apply the LINPRED in case it is needed
                  LINPRED_LAG_VAR <- spm_formulas(object)@lag_vars

                  if (!is.null(LINPRED_LAG_VAR)){
                    mats <- LINPRED(data_filtered,
                                    spm_boundaries(object),
                                    time_col, LINPRED_LAG_VAR,
                                    k = 5, m = 1)$vars

                    # Modify the mats to fit the timestep under scrutiny
                    by_mat_nrow <- dim(mats$by_matrix)[1]
                    mats$lag_matrix <- mats$lag_matrix[1:grid_length,]
                    mats$by_matrix <- mats$by_matrix[(by_mat_nrow-grid_length+1):by_mat_nrow,]

                  } else {
                    mats <- NULL
                  }

                  # Calculate ratio, then density, and format the output
                  ratio_next_ts <-
                    exp(mgcv::predict.bam(spm_get_fit(object),
                                          newdata = append(as.list(new_data), mats)))

                  density_last_year <- spm_smoothed_data(object) %>%
                    dplyr::filter(.data$year_f %in% max_ts) %>%
                    dplyr::pull(.data[[biomass]])

                  preds_df <- patches %>%
                    dplyr::mutate(density_next_ts = density_last_year * ratio_next_ts,
                                  biomass = .data$density_next_ts *
                                    (as.numeric(units::set_units(st_area(patches), value = "km^2"))),
                                  year_f = next_ts) %>%
                    dplyr::select(-.data$density_next_ts) %>%
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
                    dplyr::mutate(area = st_area(.data$geometry)) %>%
                    dplyr::mutate(area = units::set_units(.data$area,
                                                          value = "km^2")) %>%
                    dplyr::relocate(.data$area, .before = "geometry")

                  preds_df <- pred_subset %>%
                    cbind(data.frame(biomass_density_with_catch = biomass_density_with_catch,
                                     biomass_density =  biomass_pred)) %>%
                    dplyr::mutate(biomass_with_catch = .data$biomass_density_with_catch *
                                    as.numeric(.data$area),
                                  biomass = .data$biomass_density *
                                    as.numeric(.data$area)) %>%
                    dplyr::relocate(.data$biomass_with_catch, .data$biomass,
                                    .before = "geometry") %>%
                    sf::st_as_sf()

                }

              }

            }

            return(preds_df)
          }
)

# -------------------------------------------------------------------------

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

get_lagged_var_names <- function(sspm_object){

  all_var_names <- get_var_names(sspm_object)
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
