# Exported ----------------------------------------------------------------

#' Get the list of available discretization methods
#'
#' Currently, only one discretization method is supported:
#'     * `"tesselate_voronoi"` Voronoi tessellation using the function
#'       [tesselate_voronoi][tesselate_voronoi].
#'
#' You can create your own methid using TODO.
#'
#' @return
#' A `character vector` of all available discretization methods.
#'
#' @export
spm_methods <- function(){
  choices <- c('tesselate_voronoi')
  return(choices)
}

# Not exported ------------------------------------------------------------

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

dispatch_method <- function(discretization_method){

  checkmate::assert_character(discretization_method)

  if (discretization_method == "tesselate_voronoi"){
    return(tesselate_voronoi)
  } else {
    stop()
  }
}

suppressAll <- function(x){
  suppressWarnings(suppressMessages(x))
}

# Print helpers -----------------------------------------------------------

cat_boundaries <- function(object){
  cli::cli_h3(cli::col_cyan("Boundaries"))
  cli::cat_bullet(" Boundary data    : ", " Simple feature collection with ",
                  dim(object@boundaries)[1] ," feature(s) and ",
                  dim(object@boundaries)[2], " variable(s)")
}

cat_discretization_info <- function(object){
  cli::cli_h3(cli::col_cyan("Discretization info"))
  cli::cat_bullet(" Method name      : '", object@method@name, "'")
  cli::cat_bullet(" Patches          : ", "Simple feature collection with ",
                  dim(object@patches)[1] ," patches (and ",
                  dim(object@patches)[2], " field(s))")
  cli::cat_bullet(" Points           : ", "Simple feature collection with ",
                  dim(object@points)[1] ," points (and ",
                  dim(object@points)[2], " field(s))")
}

cat_mapped_datasets <- function(object){
  cli::cli_h3(cli::col_cyan("Mapped Datasets"))
  cat_bullet(" TODO")
}

message_not_discrete <- function(object){
  message(paste0("Model object '", spm_name(object),
                 "' is not a discrete model. See ?spm_discretize for discretization methods"))
}
