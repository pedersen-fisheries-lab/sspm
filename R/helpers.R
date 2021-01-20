#' @export
length_to_weigth <- function(length, sex){

  checkmate::assert_character(sex)
  checkmate::assert_numeric(length)

  weigth <- ifelse(sex=="male",
                   0.00088*length^2.857,
                   0.00193*length^2.663)
  return(weigth)
}

# Using statmod package, this has functions for randomized quantile residuals
#' @export
rqresiduals <- function (gam.obj) {

  checkmate::assert_class(gam.obj, "gam")
  check_model_family(gam.obj$family$family)

  if (grepl("^Tweedie", gam.obj$family$family)) {

    if (is.null(environment(gam.obj$family$variance)$p)) {
      p.val <- gam.obj$family$getTheta(TRUE)
      environment(gam.obj$family$variance)$p <- p.val
    }

    qres <- statmod::qres.tweedie(gam.obj)

  } else if (grepl("^Negative Binomial", gam.obj$family$family)) {

    if ("extended.family" %in% class(gam.obj$family)) {
      gam.obj$theta <- gam.obj$family$getTheta(TRUE)
    } else {
      gam.obj$theta <- gam.obj$family$getTheta()
    }

    qres <- statmod::qres.nbinom(gam.obj)

  } else {

    qres <- statmod::qresid(gam.obj)

  }
  return(qres)
}

# This should be simple enough to test (use mgcv gam example code)
#' @export
check_model_family <- function(family){

  checkmate::check_class(family, "family")

  if (!grepl("^Tweedie|^Negative Binomial|^poisson|^binomial|^gaussian|^Gamma|^inverse.gaussian",
             family)){
    stop(paste0("family " , family,
                " is not currently supported by the statmod library,
               and any randomized quantile residuals would be inaccurate."))
  }
}
