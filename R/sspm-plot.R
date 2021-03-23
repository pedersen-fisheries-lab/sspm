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
            par(mfrow=c(1,2))
            plot(spm_patches(x)$geometry, main = "Patches")
            plot(spm_points(x)$geometry, main = "Points")
            par(old_par)

          }
)
