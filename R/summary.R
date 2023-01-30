#' Summarises `sspm_fit` objects
#'
#' Summarises a `sspm_fit` object, both in terms of productivity and biomass.
#'
#' @param object **\[sspm_...\]** An object from this package.
#' @param biomass **\[character\]** Biomass variable.
#'
#' @return
#' A summary.
#'
#' @examples
#' \dontrun{
#' summary(sspm_model_fit)
#' summary(sspm_model_fit, biomass = "weight_per_km2_borealis")
#' }
#'
#' @export
#' @name summary
#' @aliases summary.sspm

NULL

#' @rdname summary
setMethod("summary",
          signature(object = "sspm_fit"),
          definition = function(object, biomass = NULL) {

            # time_var <- spm_time(object)
            boundary_var <- spm_boundary(object)
            patches_area_var <- spm_patches_area(spm_boundaries(object))

            prod_sum <- predict(object) %>%
              sf::st_drop_geometry() %>%
              dplyr::select(pred, dplyr::all_of(boundary_var)) %>%
              dplyr::group_by(.data[[boundary_var]]) %>%
              dplyr::summarise(mean = mean(pred),
                               min = min(pred),
                               max = max(pred),
                               sd = sd(pred)) %>%
              dplyr::ungroup() %>%
              as.data.frame()

            if (is.null(biomass)){

              cli::cat_rule("Productivity")
              cli::cat_print(prod_sum)

            } else {

              bio_den_sum <- predict(object, biomass = biomass) %>%
                sf::st_drop_geometry() %>%
                dplyr::select(biomass_density, dplyr::all_of(boundary_var)) %>%
                dplyr::group_by(.data[[boundary_var]]) %>%
                dplyr::summarise(mean = mean(biomass_density, na.rm = TRUE),
                                 min = min(biomass_density, na.rm = TRUE),
                                 max = max(biomass_density, na.rm = TRUE),
                                 sd = sd(biomass_density, na.rm = TRUE)) %>%
                dplyr::ungroup() %>%
                as.data.frame()

              bio_sum <- predict(object, biomass = biomass) %>%
                sf::st_drop_geometry() %>%
                dplyr::select(biomass_with_catch, dplyr::all_of(boundary_var)) %>%
                dplyr::group_by(.data[[boundary_var]]) %>%
                dplyr::summarise(mean = mean(biomass_with_catch, na.rm = TRUE),
                                 min = min(biomass_with_catch, na.rm = TRUE),
                                 max = max(biomass_with_catch, na.rm = TRUE),
                                 sd = sd(biomass_with_catch, na.rm = TRUE)) %>%
                dplyr::ungroup() %>%
                as.data.frame()

              cli::cat_rule("Productivity")
              cli::cat_print(prod_sum)
              cli::cat_rule("Biomass density")
              cli::cat_print(bio_den_sum)
              cli::cat_rule("Biomass (with catch)")
              cli::cat_print(bio_sum)
            }

          }
)
