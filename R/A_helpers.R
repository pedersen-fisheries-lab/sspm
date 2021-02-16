# Exported ----------------------------------------------------------------

NULL

# Not exported ------------------------------------------------------------

# Convert shrimp length to weight
length_to_weigth <- function(length, sex){

  checkmate::assert_character(sex)
  checkmate::assert_numeric(length)

  weigth <- ifelse(sex=="male",
                   0.00088*length^2.857,
                   0.00193*length^2.663)
  return(weigth)
}

# This should be simple enough to test (use mgcv gam example code)
check_model_family <- function(family){

  checkmate::check_class(family, "family")

  if (!grepl("^Tweedie|^Negative Binomial|^poisson|^binomial|^gaussian|^Gamma|^inverse.gaussian",
             family)){
    stop(paste0("family " , family,
                " is not currently supported by the statmod library,
               and any randomized quantile residuals would be inaccurate."))
  }
}

# Using statmod package, this has functions for randomized quantile residuals
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

# Suppress both messages and warnings
suppressAll <- function(x){
  suppressWarnings(suppressMessages(x))
}

# Prints out error that an object is not a discrete object
message_not_discrete <- function(object){
  cli::cli_alert_danger(paste0(" Model object '", spm_name(object),
                               "' is not a discrete model"))
  cli::cli_alert_info(" See ?spm_discretize for discretization methods")
}

# Extract the base smooth type form a mgcv smooth object
get_base_smooth_type <- function(object){
  type <- gsub(".smooth.spec", "", class(object), fixed = TRUE)
  return(type)
}

# Returns TRUE if it is a mgcv smooth, FALSE otherwise
# object = expected to be the mgcv smooth object
is_smooth_spec <- function(object){
  checked_smooth  <- grepl("smooth.spec", class(object), fixed = TRUE)
  return(checked_smooth)
}
