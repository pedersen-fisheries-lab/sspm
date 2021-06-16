#' Plot various `sspm` objects
#'
#' Plot methods for a range of sspm objects.
#'
#' @param sspm_object An object from this package.
#' @param smoothed_var TODO
#' @param page TODO
#' @param nrow TODO
#' @param ncol TODO
#'
#' @return
#' N/A
#'
#' @export
setGeneric(name = "spm_plot",
           def = function(sspm_object, smoothed_var = NULL,
                          page = NULL, nrow = NULL, ncol = NULL){
             standardGeneric("spm_plot")
           }
)

# Methods -----------------------------------------------------------------

#' @export
setMethod("spm_plot",
          signature(sspm_object = "sspm_boundary"),
          definition = function(sspm_object) {

            boundaries <- spm_boundaries(sspm_object)
            boundary_column <- spm_boundary_colum(sspm_object)

            if (checkmate::test_class(sspm_object, "sspm_discrete_boundary")){

              patches <- sspm_object@patches
              points <- sspm_object@points

              sspm_discrete_plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = patches,
                                 fill = NA, col = "#36454F") +
                ggplot2::geom_sf(data = points,
                                 col ="#6082B6") +
                ggplot2::geom_sf(data = boundaries,
                                 ggplot2::aes(col = .data[[boundary_column]]),
                                 fill = NA) +
                ggplot2::scale_color_viridis_d(boundary_column) +
                ggplot2::theme_light()

            } else if(checkmate::test_class(sspm_object, "sspm_boundary")){

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
setMethod("spm_plot",
          signature(sspm_object = "sspm_data"),
          definition = function(sspm_object, smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4) {

            smoothed_data <- spm_smoothed_data(sspm_object)

            if(is.null(smoothed_var)){

              sspm_discrete_plot <- plot(spm_boundaries(sspm_object))
              show(sspm_discrete_plot)

            } else {

              # TODO add check for smoothed_var to be in smoothed_data

              time_col <- spm_time_column(sspm_object)

              sspm_discrete_plot <- spm_plot_routine(smoothed_data, smoothed_var,
                                                     page, nrow, ncol, time_col)

              return(sspm_discrete_plot)
            }

          }
)

#' @export
setMethod("spm_plot",
          signature(sspm_object = "sspm_fit"),
          definition = function(sspm_object, smoothed_var = NULL,
                                page = "first", nrow = 2, ncol = 4) {

            smoothed_data <- spm_smoothed_data(sspm_object)

            if(!is.null(smoothed_var)){

              # TODO add check for smoothed_var to be in smoothed_data

              time_col <- spm_time_column(sspm_object)

              sspm_discrete_plot <- spm_plot_routine(smoothed_data, smoothed_var,
                                                     page, nrow, ncol, time_col)

            } else {

              preds <- spm_predict(sspm_object)
              response <- spm_response(spm_formulas(sspm_object))
              smoothed_data_with_preds <- smoothed_data %>%
                dplyr::mutate(predicted = preds,
                              color = ifelse(train_test, "TRAIN", "TEST"))

              sspm_discrete_plot <-
                ggplot2::ggplot(data = smoothed_data_with_preds) +
                ggplot2::geom_point(ggplot2::aes(x = .data[[response]], y = predicted, col = color)) +
                ggplot2::theme_light() +
                ggplot2::labs(x="actual") +
                ggplot2::scale_color_viridis_d("Set")

            }

            return(sspm_discrete_plot)

          }
)

# -------------------------------------------------------------------------

spm_plot_routine <- function(smoothed_data, smoothed_var,
                             page, nrow, ncol, time_col){

  if(is.character(page)){

    if (page == "all"){

      time_col_levels <- length(unique(smoothed_data[[time_col]]))
      n_per_page <- nrow*ncol
      n_pages <- time_col_levels %/% (n_per_page) +
        (time_col_levels %% n_per_page > 1)

      sspm_discrete_plot <- list()

      for (page_nb in seq_len(length.out = n_pages)){

        sspm_discrete_plot[[page_nb]] <-
          ggplot2::ggplot(data = smoothed_data) +
          ggplot2::geom_sf(ggplot2::aes(fill=.data[[smoothed_var]])) +
          ggforce::facet_wrap_paginate(~.data[[time_col]],
                                       nrow = nrow, ncol = ncol,
                                       page = page_nb) +
          ggplot2::scale_fill_viridis_c()

      }

    } else {

      sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
        ggplot2::geom_sf(ggplot2::aes(fill=.data[[smoothed_var]])) +
        ggforce::facet_wrap_paginate(~.data[[time_col]],
                                     nrow = nrow, ncol = ncol,
                                     page = 1) +
        ggplot2::scale_fill_viridis_c()

    }

  } else if(is.numeric(page)){

    sspm_discrete_plot <- ggplot2::ggplot(data = smoothed_data) +
      ggplot2::geom_sf(ggplot2::aes(fill=.data[[smoothed_var]])) +
      ggforce::facet_wrap_paginate(~.data[[time_col]],
                                   nrow = nrow, ncol = ncol,
                                   page = page) +
      ggplot2::scale_fill_viridis_c()

  }

  return(sspm_discrete_plot)

}
