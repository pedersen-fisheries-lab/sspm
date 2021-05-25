#' Plot a discrete `sspm` model object
#'
#' This will plot the patches and points of a `sspm_discrete` object.
#'
#' @param x **\[sspm_discrete\]** An object of class
#'     [sspm_discrete][sspm_discrete-class].
#'
#' @return
#' N/A
#'
#' @name plot.sspm
#' @rdname plot-sspm

NULL

#' @rdname plot-sspm
#' @export
setMethod("plot",
          signature(x = "sspm"),
          definition = function(x) {
            cli::cli_alert_danger(" This model is not discretized and cannot be plotted")
          }
)

#' @rdname plot-sspm
#' @export
setMethod("plot",
          signature(x = "sspm_discrete"),
          definition = function(x) {

            old_par <- par(no.readonly = TRUE)

            sspm_discrete_plot <- ggplot2::ggplot() +
              ggplot2::geom_sf(data = spm_patches(x),
                               fill = NA, col = "#8A9A5B") +
              ggplot2::geom_sf(data = spm_boundaries(x),
                               fill = NA, col = "#36454F") +
              ggplot2::geom_sf(data = spm_points(x),
                               col ="#6082B6") +
              ggplot2::theme_light()

            sspm_discrete_plot

          }
)
