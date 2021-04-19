# Exported ----------------------------------------------------------------

#' Get the list of available discretization methods
#'
#' Currently, only one discretization method is supported:
#'     * `"tesselate_voronoi"` Voronoi tessellation using the function
#'       [tesselate_voronoi][tesselate_voronoi].
#'
#' You can create your own method (tutorial TBD).
#'
#' @return
#' A `character vector` of all available discretization methods.
#'
#' @export
spm_methods <- function(){
  choices <- c('tesselate_voronoi')
  return(choices)
}

#' Get the list of available smoothing methods
#'
#' Currently, only one smoothing method is supported:
#'     * `"ICAR"`: Intrinsic Conditional Auto-Regressive models.
#'
#' @return
#' A `character vector` of all available smoothing methods.
#'
#' @export
spm_smooth_methods <- function(){
  choices <- c('ICAR')
  return(choices)
}

# Not exported ------------------------------------------------------------

# Join datasets to patches
join_datasets <- function(sspm_data, sspm_object){

  checkmate::assert_class(sspm_data, "sspm_data")
  checkmate::assert_class(sspm_object, "sspm_discrete")

  the_data <- spm_data(sspm_data)
  the_patches <- spm_patches(sspm_object)

  # TODO REVIEW THE COHERENCE OF ST_TRANSFORM
  joined <- suppressMessages(sf::st_transform(the_data, crs = sf::st_crs(the_patches)))
  joined <- suppressMessages(sf::st_join(the_data, the_patches)) %>%
    dplyr::filter(!duplicated(.data[[spm_unique_ID(sspm_data)]])) %>%
    dplyr::filter(!is.na(.data$patch_id))

  spm_data(sspm_data) <- joined

  return(sspm_data)
}

# Dispatch the correct function based on the name of the method
dispatch_method <- function(discretization_method){

  checkmate::assert_character(discretization_method)

  if (discretization_method == "tesselate_voronoi"){
    return(tesselate_voronoi)
  } else {
    cli::cli_alert_danger(paste0("Method '", discretization_method,
                                 "' is not part of the supported methods."))
    cli::cli_alert_info("See `?spm_methods()`")
  }
}

# Dispatch the correct function based on the name of the method
dispatch_smooth <- function(smooth_method){

  checkmate::assert_character(smooth_method)

  if (smooth_method == "ICAR"){
    return(ICAR)
  } else {
    cli::cli_alert_danger(paste0("Smoothing method '", smooth_method,
                                 "' is not part of the supported methods."))
    cli::cli_alert_info("See `?spm_smooth_methods()`")
  }
}

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

  if (!any(grepl("^Tweedie|^Negative Binomial|^poisson|^binomial|^gaussian|^Gamma|^inverse.gaussian",
                 family))){
    stop(paste0("family " , family,
                " is not currently supported by the statmod library,
               and any randomized quantile residuals would be inaccurate."),
         call. = FALSE)
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

# This functions takes a list of arguments args and modify the call of another
# function as to add those arguments. This is necessary to pass key arguments
# to the smoothing functions
modify_call <- function(the_call, args){
  for (index in seq_len(length(args))){
    the_call[[names(args)[index]]] <- args[[index]]
  }
  return(the_call)
}

# This function generates multilag values for a given vector
multilag <-  function(variable, n_lags, default = NA){
  out_mat <- sapply(1:n_lags, FUN = lag, x=variable, default = default)
  colnames(out) <- paste0("lag", 1:n_lags)
  as.data.frame(out)
}
