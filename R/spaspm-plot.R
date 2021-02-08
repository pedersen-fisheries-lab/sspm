#' Plot a discrete `spaspm` model object
#'
#' This will plot the patches and points of a `spaspm_discrete` object.
#'
#' @param x **\[spaspm_discrete\]** An object of class
#'     [spaspm_discrete][spaspm_discrete-class].
#'
#' @return
#' N/A
#'
#' @name plot.spaspm
#' @rdname plot-spaspm

NULL

#' @describeIn plot-spaspm TODO
#' @export
setMethod("plot",
          signature(x = "spaspm"),
          definition = function(x) {
            cli::cli_alert_danger(" This model is not discretized and cannot be plotted")
          }
)

#' @describeIn plot-spaspm TODO
#' @export
setMethod("plot",
          signature(x = "spaspm_discrete"),
          definition = function(x) {

            old_par <- par(no.readonly = TRUE)
            par(mfrow=c(1,2))
            plot(spm_patches(x)$geometry, main = "Patches")
            plot(spm_points(x)$geometry, main = "Points")
            par(old_par)

          }
)
