#' Smooth a variable in a sspm dataset
#'
#' With a formula, smooth a variable in a sspm dataset. See Details for
#' more explanations.
#'
#' @param sspm_object **\[sspm_dataset\]** An object of class
#'     [sspm_dataset][sspm_dataset-class].
#' @param formula **\[formula\]** A formula definition of the form
#'     response ~ smoothing_terms + ...
#' @param boundaries **\[sspm_boundary\]** An object of class
#'     [sspm_discrete_boundary][sspm_boundary-class].
#' @param keep_fit **\[logical\]** Whether or not to keep the fitted values and
#'     model (default to TRUE, set to FALSE to reduce memory footprint).
#' @param predict **\[logical\]** Whether or not to generate the smoothed
#'     predictions (necessary to fit the final SPM model, default to TRUE).
#' @inheritDotParams mgcv::bam
#'
#' @details
#' This functions allows to specify a model formula for a given discrete sspm
#' object. The formula makes use of specific smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`. The formula can also contain fixed
#' effects and custom smooths, and can make use of specific smoothing terms
#' `smooth_time()`, `smooth_space()`, `smooth_space_time()`.
#'
#' @export
setGeneric(name = "spm_smooth",
           def = function(sspm_object,
                          formula,
                          boundaries,
                          keep_fit = TRUE,
                          predict = TRUE,
                          ...) {
             standardGeneric("spm_smooth")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname map_formula
setMethod(f = "spm_smooth",
          signature(sspm_object = "ANY",
                    formula = "missing",
                    boundaries = "ANY"),
          function(sspm_object, formula, boundaries, keep_fit, predict, ...) {
            cli::cli_alert_danger(" Argument 'formula' missing with no default")
          }
)

#' @export
#' @rdname map_formula
setMethod(f = "spm_smooth",
          signature(sspm_object = "ANY",
                    formula = "ANY",
                    boundaries = "missing"),
          function(sspm_object, formula, boundaries, keep_fit, predict, ...) {

            if (!is_mapped(sspm_object)) {
              cli::cli_alert_danger(" Argument 'boundaries' missing with no default")
            } else {
              boundaries <- spm_boundaries(sspm_object)
            }

             spm_smooth(sspm_object, formula, boundaries, keep_fit, predict, ...)

          }
)

#' @export
#' @rdname map_formula
setMethod(f = "spm_smooth",
          signature(sspm_object = "ANY",
                    formula = "ANY",
                    boundaries = "sspm_boundary"),
          function(sspm_object, formula, boundaries, keep_fit, predict, ...) {
            cli::cli_alert_danger(" Argument 'boundaries' must have been discretized")
          }
)

#' @export
#' @rdname spm_smooth
setMethod(f = "spm_smooth",
          signature(sspm_object = "sspm_dataset",
                    formula = "formula",
                    boundaries = "sspm_discrete_boundary"),
          function(sspm_object, formula, boundaries, keep_fit, predict, ...) {

            # 1. Map boundary data
            if (!is_mapped(sspm_object)) {
              sspm_object_joined <- join_datasets(sspm_object, boundaries)
            } else {
              sspm_object_joined <- sspm_object
            }

            # 2. call map_formula
            data_frame <- spm_data(sspm_object_joined)
            time_column <- spm_time_column(sspm_object_joined)

            sspm_formula <- map_formula(data_frame = data_frame,
                                        boundaries = boundaries,
                                        formula = formula,
                                        time_column = time_column,
                                        ...)

            # Check that response is a density
            response <- spm_response(sspm_formula)
            if (!checkmate::test_choice(response, sspm_object@vars_density)){
              cli::cli_alert_warning(
                paste0(" Response variable ", cli::col_br_red(response),
                       " is NOT a biomass density variable"))
              stop("spm_smooth is designed for modelling density variables")
            }

            spm_formulas(sspm_object_joined) <-
              append(spm_formulas(sspm_object_joined), list(sspm_formula))

            # 3. call fit with ... arguments
            sspm_object_fitted <- sspm_object_joined %>%
              fit_smooths(boundaries = boundaries, keep_fit = keep_fit, ...)

            # 4. if predict, also generate predictions
            if(predict){

              preds_df <- predict(sspm_object_fitted)

              sspm_object_fitted <-
                join_smoothed_datasets(sspm_object_fitted, preds_df)

            }

            return(sspm_object_fitted)
          }
)

# Helpers -----------------------------------------------------------------

# Takes care of joining things when prediction is made
join_smoothed_datasets <- function(sspm_object, preds_df){

  smoothed_data <- spm_smoothed_data(sspm_object)
  time_col <- spm_time_column(sspm_object)
  boundaries <- spm_boundaries(sspm_object)
  patches <- spm_patches(boundaries)

  if (is.null(smoothed_data)) {
    smoothed_data <- data.frame()
  }

  if (nrow(smoothed_data) == 0) {

    smoothed_data <- preds_df %>%
      dplyr::left_join(patches, by = c("patch_id"),
                       suffix = c("", "_duplicate")) %>%
      dplyr::select(-c(dplyr::ends_with("_duplicate")))

  } else {

    smoothed_data <- preds_df %>%
      dplyr::left_join(smoothed_data, by = c("patch_id", time_col),
                       suffix = c("", "_duplicate")) %>%
      dplyr::select(-c(dplyr::ends_with("_duplicate")))

  }

  nrow_smoothed_data <- nrow(smoothed_data)
  smoothed_data <-
    smoothed_data %>%
    dplyr::mutate("row_ID" = 1:nrow_smoothed_data) %>%
    dplyr::relocate(.data$row_ID) %>%
    sf::st_as_sf() # TODO check CRS

  spm_smoothed_data(sspm_object) <- smoothed_data

  return(sspm_object)
}
