#' Plot various `sspm` objects
#'
#' Plot methods for a range of sspm objects.
#'
#' @param sspm_object **\[sspm_...\]** An object from this package.
#' @param smoothed_var **\[character\]** Variable to plot.
#' @param page **\[character\]** Either "first" for the firs page of plots, or
#'     "all" for all pages
#' @param nrow **\[numeric\]** The number of rows to paginate the plot on.
#' @param ncol **\[numeric\]** The number of columns to paginate the plot on.
#' @param log **\[logical\]** Whether to plot on a log scale, default to TRUE.
#'
#' @return
#' N/A
#'
#' @export
setGeneric(name = "spm_plot",
           def = function(sspm_object, smoothed_var = NULL,
                          page = NULL, nrow = NULL, ncol = NULL, log = NULL) {
             standardGeneric("spm_plot")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_plot
setMethod("spm_plot",
          signature(sspm_object = "sspm_boundary"),
          definition = function(sspm_object) {

            boundaries <- spm_boundaries(sspm_object)
            boundary_column <- spm_boundary_colum(sspm_object)

            if (checkmate::test_class(sspm_object, "sspm_discrete_boundary")) {

              patches <- sspm_object@patches
              points <- sspm_object@points

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = patches,
                                 fill = NA, col = "#36454F") +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(col = .data[[boundary_column]]),
                                 fill = NA) +
                ggplot2::scale_color_viridis_d(boundary_column) +
                ggplot2::theme_light()

              if(!is.null(points)){
                sspm_discrete_plot <- sspm_discrete_plot +
                  ggplot2::geom_sf(data = points, col = "#6082B6")
              }

            } else if (checkmate::test_class(sspm_object, "sspm_boundary")) {

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(fill = .data[[boundary_column]]),
                                 col = "#36454F") +
                ggplot2::scale_fill_viridis_d(boundary_column) +
                ggplot2::theme_light()

            }

            return(sspm_discrete_plot)

          }
)

#' @export
#' @rdname spm_plot
setMethod("spm_plot",
          signature(sspm_object = "sspm_dataset"),
          definition = function(sspm_object, smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4, log = TRUE) {

            smoothed_data <- spm_smoothed_data(sspm_object)

            if (is.null(smoothed_var)) {

              sspm_discrete_plot <- plot(spm_boundaries(sspm_object))
              show(sspm_discrete_plot)

            } else {

              if (!checkmate::test_subset(smoothed_var, names(smoothed_data))) {
                stop("`smoothed_var` must be a column of the smoothed data", call. = FALSE)
              }

              time_col <- spm_time_column(sspm_object)

              sspm_discrete_plot <- spm_plot_routine(smoothed_data, smoothed_var,
                                                     page, nrow, ncol, time_col, log)

              return(sspm_discrete_plot)
            }

          }
)

#' @export
#' @rdname spm_plot
setMethod("spm_plot",
          signature(sspm_object = "sspm_fit"),
          definition = function(sspm_object, smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4) {

            smoothed_data <- spm_smoothed_data(sspm_object)

            if (!is.null(smoothed_var)) {

              if (!checkmate::test_subset(smoothed_var, names(smoothed_data))) {
                stop("`smoothed_var` must be a column of the smoothed data", call. = FALSE)
              }

              time_col <- spm_time_column(sspm_object)

              sspm_discrete_plot <- spm_plot_routine(smoothed_data, smoothed_var,
                                                     page, nrow, ncol, time_col)

            } else {

              preds <- spm_predict(sspm_object)$pred
              response <- spm_response(spm_formulas(sspm_object))
              smoothed_data_with_preds <- smoothed_data %>%
                dplyr::mutate(predicted = preds,
                              color = ifelse(.data$train_test, "TRAIN", "TEST"))

              sspm_discrete_plot <-
                ggplot2::ggplot(data = smoothed_data_with_preds) +
                ggplot2::geom_point(ggplot2::aes(x = exp(.data[[response]]),
                                                 y = .data$predicted,
                                                 col = .data$color)) +
                ggplot2::theme_light() +
                ggplot2::labs(x = "actual") +
                ggplot2::scale_color_viridis_d("Set") +
                ggplot2::facet_wrap(~.data[[spm_boundary_colum(spm_boundaries(sspm_object))]])

            }

            return(sspm_discrete_plot)

          }
)

# -------------------------------------------------------------------------

spm_plot_routine <- function(smoothed_data, smoothed_var,
                             page, nrow, ncol, time_col, log) {

  if (log) {
    smoothed_data[[smoothed_var]] <- log(smoothed_data[[smoothed_var]])
    the_title <- paste0(smoothed_var, " (log)")
  } else {
    the_title <- smoothed_var
  }

  if (is.character(page)) {

    if (page == "all") {

      time_col_levels <- length(unique(smoothed_data[[time_col]]))
      n_per_page <- nrow * ncol
      n_pages <- time_col_levels %/% (n_per_page) +
        (time_col_levels %% n_per_page > 1)

      sspm_discrete_plot <- list()

      for (page_nb in seq_len(length.out = n_pages)) {

        sspm_discrete_plot[[page_nb]] <-
          ggplot2::ggplot(data = smoothed_data) +
          ggplot2::geom_sf(ggplot2::aes(fill = .data[[smoothed_var]])) +
          ggforce::facet_wrap_paginate(~ .data[[time_col]],
                                       nrow = nrow, ncol = ncol,
                                       page = page_nb) +
          ggplot2::scale_fill_viridis_c() +
          ggplot2::labs(fill = the_title)

      }

    } else {

      sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data[[smoothed_var]])) +
        ggforce::facet_wrap_paginate(~ .data[[time_col]],
                                     nrow = nrow, ncol = ncol,
                                     page = 1) +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::labs(fill = the_title)

    }

  } else if (is.numeric(page)) {

    sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[smoothed_var]])) +
      ggforce::facet_wrap_paginate(~ .data[[time_col]],
                                   nrow = nrow, ncol = ncol,
                                   page = page) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(fill = the_title)

  }

  return(sspm_discrete_plot)

}
