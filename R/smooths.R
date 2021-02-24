#' SPASPM Smoothing functions
#'
#' A full spaspm formula contains calls to the smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`.
#'
#' @param type **\[character\]** Type of smooth, currently only "ICAR" is
#'     supported.
#' @inheritParams map_formula
#'
#' @return
#' A list of 2 lists:
#' * `args`, contains the arguments to be passed on to the mgcv smooths
#' * `vars`, contains variables relevant to the evaluation of the smooth.
#'
#' @rdname smooths
#' @export
setGeneric(name = "smooth_time",
           def = function(type = "ICAR",
                          dataset,
                          spaspm_object,
                          ...){
             standardGeneric("smooth_time")
           }
)

#' @export
#' @describeIn smooths TODO
setGeneric(name = "smooth_space",
           def = function(type = "ICAR",
                          dataset,
                          spaspm_object,
                          ...){
             standardGeneric("smooth_space")
           }
)

#' @export
#' @describeIn smooths TODO
setGeneric(name = "smooth_space_time",
           def = function(type = "ICAR",
                          dataset,
                          spaspm_object,
                          ...){
             standardGeneric("smooth_space_time")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn smooths TODO
setMethod(f = "smooth_time",
          signature(type = "ANY",
                    dataset = "character",
                    spaspm_object = "spaspm_discrete"),
          function(type, dataset, spaspm_object, ...){

            # Get args from ellipsis add time_col as the first element by appending
            args <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            if(!is.null(type)){
              if(type == "ICAR"){
                args_and_vars <- do.call(ICAR,
                                         append(list(spaspm_object = spaspm_object,
                                                     dataset = dataset,
                                                     dimension = "time"),
                                                args))
              } else {
                stop("Smooth type provided not supported")
              }
            }

            return(args_and_vars)

          }
)

# Accessory functions -----------------------------------------------------

# This functions turns the args_and_vars returned by ICAR (and potentially any
# any other functions like ICAR) into a call to s()
assemble_smooth <- function(args_and_vars){
  deparse(rlang::call2("s", !!!args_and_vars$args),
          width.cutoff = 500, nlines = 1)
}

# Construct an ICAR penalization matrix for a given "dimension" and returns the
# double list args_and_vars that have the args to build a new call to s() and the
# vars necessary for the evaluation of that s() smooth
ICAR <- function(spaspm_object, dataset, dimension, column,
                 k = 30, bs = "re", ...){

  # Recapture the ellipsis again
  args <- as.list(match.call(expand.dots = FALSE)$`...`)

  # Get data/dataset
  the_dataset <- spm_datasets(spaspm_object)[[dataset]]
  the_data <- spm_data(the_dataset)

  vars <- list()
  if (dimension == "time") {

    column <- spm_time_col(spm_datasets(spaspm_object)[[dataset]])

    # Creating an auto-regressive year penalty; this matrix means that the
    # estimate for each year is penalized to be close to the years before and
    # after it

    time_levels <- unique(the_data[[column]])
    n_time_levels = length(time_levels)

    pen_mat = matrix(0, nrow=n_time_levels, ncol = n_time_levels)
    dimnames(pen_mat) = list(time_levels, time_levels)

    diag(pen_mat[-1,-n_time_levels]) = diag(pen_mat[-n_time_levels,-1]) = -1
    diag(pen_mat) = -(colSums(pen_mat)-diag(pen_mat))

    pen_expression <- rlang::expr(pen_mat_time)
    vars$pen_mat_time <- pen_mat

  } else if (dimension == "space"){

  }

  return(list(args = append(list(str2lang(column),
                                 k = k,
                                 bs = bs,
                                 xt = list(penalty = pen_expression)),
                            args),
              vars = vars))
}
