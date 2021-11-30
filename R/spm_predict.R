#' Predict with a SPM model
#'
#' Predict using a fitted SPM model on the whole data or on new data
#'
#' @inheritParams spm_smooth
#' @param new_data **\[data.frame\]** New data to predict with.
#' @param biomass **\[data.frame\]** Biomass variable.
#' @param ... Arguments passed on to [predict.bam].
#'
#' @return
#' A `dataframe` of predictions.
#'
#' @export
#' @describeIn spm_predict Productivity predictions from a ssspm_fit object.
setGeneric(name = "spm_predict",
           def = function(sspm_object,
                          new_data = NULL,
                          ...) {
             standardGeneric("spm_predict")
           }
)

#' @export
#' @describeIn spm_predict Biomass predictions from a ssspm_fit object.
setGeneric(name = "spm_predict_biomass",
           def = function(sspm_object,
                          biomass = NULL,
                          ...) {
             standardGeneric("spm_predict_biomass")
           }
)

#' @export
#' @describeIn spm_predict Biomass predictions from a ssspm_fit object.
setGeneric(name = "spm_predict_biomass_next_timestep",
           def = function(sspm_object,
                          biomass = NULL,
                          ...) {
             standardGeneric("spm_predict_biomass_next_timestep")
           }
)


# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict",
          signature(sspm_object = "sspm_fit",
                    new_data = "data.frame"),
          function(sspm_object, new_data, ...) {
            new_data <- as.list(new_data)
            spm_predict(sspm_object = sspm_object, new_data = new_data, ...)
          }
)

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict",
          signature(sspm_object = "sspm_fit",
                    new_data = "missing"),
          function(sspm_object, new_data, ...) {
            new_data <- append(as.list(spm_smoothed_data(sspm_object)),
                               formula_vars(spm_formulas(sspm_object)))
            spm_predict(sspm_object = sspm_object, new_data = new_data, ...)
          }
)

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict",
          signature(sspm_object = "sspm_fit",
                    new_data = "list"),
          function(sspm_object, new_data, ...) {

            pred_log <- spm_get_fit(sspm_object) %>%
              predict.bam(newdata = new_data, type = "response", ...)
            preds_df <- data.frame(pred_log = pred_log) %>%
              dplyr::mutate(pred = exp(pred_log))

            columns_to_keep <- spm_smoothed_data(sspm_object) %>%
              dplyr::select(.data$patch_id, !!spm_time_column(sspm_object),
                            !!spm_boundary_column(spm_boundaries(sspm_object)))

            preds_df <- cbind(preds_df, columns_to_keep)

            return(preds_df)
          }
)

# -------------------------------------------------------------------------

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict_biomass",
          signature(sspm_object = "sspm_fit",
                    biomass = "character"),
          function(sspm_object, biomass, ...) {

            data <- spm_smoothed_data(sspm_object)

            if (!checkmate::test_subset(biomass, names(data))) {
              stop("`biomass` must be a column of `data`", call. = FALSE)
            } else {
              biomass <- data[[biomass]]
            }

            spm_predict_biomass(sspm_object, biomass, ...)
          }
)

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict_biomass",
          signature(sspm_object = "sspm_fit",
                    biomass = "numeric"),
          function(sspm_object, biomass, ...) {

            preds <- spm_predict(sspm_object)

            biomass_density_with_catch <- preds$pred * biomass
            catch_density <- spm_smoothed_data(sspm_object)$catch_density
            biomass_pred <- biomass_density_with_catch - catch_density

            # catch_density <- NA
            # biomass_pred <- biomass_density_with_catch

            pred_subset <- dplyr::select(preds, -.data$pred, -.data$pred_log) %>%
              dplyr::mutate(area = st_area(.data$geometry)) %>%
              dplyr::mutate(area = units::set_units(.data$area,
                                                    value = "km^2")) %>%
              dplyr::relocate(.data$area, .before = "geometry")

            biomass_pred_df <- pred_subset %>%
              cbind(data.frame(biomass_density_with_catch = biomass_density_with_catch,
                               biomass_density =  biomass_pred)) %>%
              dplyr::mutate(biomass_with_catch = .data$biomass_density_with_catch *
                              as.numeric(.data$area),
                            biomass = .data$biomass_density *
                              as.numeric(.data$area)) %>%
              sf::st_as_sf()

            return(biomass_pred_df)
          }
)

#' @export
#' @rdname spm_predict
setMethod(f = "spm_predict_biomass_next_timestep",
          signature(sspm_object = "sspm_fit",
                    biomass = "character"),
          function(sspm_object, biomass, ...) {

            if (!checkmate::test_subset(biomass,
                                        names(spm_smoothed_data(sspm_object)))) {
              stop("`biomass` must be a column of `data`", call. = FALSE)
            }

            bounds <- spm_boundaries(sspm_object)
            bounds_col <- spm_boundary_column(bounds)
            time_col <- spm_time_column(sspm_object)
            base <- spm_patches(bounds)
            vars_names <- get_lagged_var_names(sspm_fit)

            all_years <- as.numeric(as.character(
              unique(spm_smoothed_data(sspm_object)[[time_col]])))
            max_year <- max(all_years)
            next_year <- max_year + 1

            new_grid <- base %>%
              dplyr::select(.data[[bounds_col]], .data$patch_id) %>%
              tidyr::expand_grid(!!time_col := 2019)
            grid_length <- nrow(new_grid)

            spm_smoothed_data(sspm_object) <- spm_smoothed_data(sspm_object) %>%
              dplyr::bind_rows(new_grid)

            sspm_object <- sspm_object %>%
              spm_lag(vars_names, 1, default = NA)

            new_data <- spm_smoothed_data(sspm_object) %>%
              filter(year_f == next_year) %>% st_drop_geometry()

            mats <- sspm:::LINPRED(spm_smoothed_data(sspm_object) %>%
                                     filter(.data[[time_col]] %in% (next_year-5):next_year),
                                   spm_boundaries(sspm_object),
                                   time_col, paste0(biomass, "_lag_1"),
                                   k = 5, m = 1)$vars

            by_mat_nrow <- dim(mats$by_matrix)[1]
            mats$lag_matrix <- mats$lag_matrix[1:grid_length,]
            mats$by_matrix <- mats$by_matrix[(by_mat_nrow-grid_length+1):by_mat_nrow,]

            ratio_next_year <- exp(predict(sspm_fit@fit,
                                           newdata = append(as.list(new_data), mats)))

            density_last_year <- spm_smoothed_data(sspm_object) %>%
              dplyr::filter(year_f %in% max_year) %>%
              pull(.data[[biomass]])

            base_pred <- base %>%
              dplyr::mutate(density_next_year = density_last_year * ratio_next_year,
                            biomass_next_year = ratio_next_year *
                              (as.numeric(units::set_units(st_area(base), value = "km^2"))),
                            year_f = next_year) %>%
              dplyr::select(-.data$density_next_year) %>%
              dplyr::relocate(.data$biomass_next_year) %>%
              dplyr::relocate(.data[[bounds_col]]) %>%
              dplyr::relocate(.data[[time_col]])

            # base_pred_sum <- base_pred %>%
            #   group_by(!!time_col, !!bounds_col) %>%
            #   mutate(biomass_next_year = sum(biomass_next_year))

            return(base_pred)
          }
)


# -------------------------------------------------------------------------

get_var_names <- function(sspm_object, exclude_mats = TRUE) {
  var_names <- sspm_object@fit$var.summary %>%
    names()
  if (exclude_mats){
    var_names <- var_names %>%
      stringr::str_subset("matrix", negate = TRUE)
  }
}

get_lagged_var_names <- function(sspm_object){
  get_var_names(sspm_object) %>%
    stringr::str_subset("_lag_1") %>%
    stringr::str_remove("_lag_1")
}
