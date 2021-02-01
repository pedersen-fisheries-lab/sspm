#' S4 plot method
#'
#' @name plot
#' @export
setMethod("plot",
          signature(x = "spaspm"),
          definition = function(x) {
            message("  This model is not discretized and cannot be plotted")
          }
)

#' @describeIn plot TODO
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
