#' sspm Smoothing functions
#'
#' A full sspm formula contains calls to the smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`.
#'
#' @param type **\[character\]** Type of smooth, currently only "ICAR" is
#'     supported.
#' @inheritParams map_formula
#' @inheritParams mgcv::s
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
                          sspm_object,
                          k = NULL,
                          bs = "re",
                          xt = NULL,
                          ...){
             standardGeneric("smooth_time")
           }
)

#' @export
#' @describeIn smooths TODO
setGeneric(name = "smooth_space",
           def = function(type = "ICAR",
                          dataset,
                          sspm_object,
                          k = 30,
                          bs = "mrf",
                          xt = NULL,
                          ...){
             standardGeneric("smooth_space")
           }
)

#' @export
#' @describeIn smooths TODO
setGeneric(name = "smooth_space_time",
           def = function(type = "ICAR",
                          dataset,
                          sspm_object,
                          k = NULL,
                          bs = c("re","mrf"),
                          xt = NULL,
                          ...){
             standardGeneric("smooth_space_time")
           }
)

# Methods -----------------------------------------------------------------

# Beware: the following code is repeated three time for the sake of user
# friendliness, although much has been done to limit repetition. If you are
# about to edit some of this code, check whether this edit should not be also
# done to the other occurence of that code.

#' @export
#' @describeIn smooths TODO
setMethod(f = "smooth_time",
          signature(type = "ANY",
                    dataset = "character",
                    sspm_object = "sspm_discrete"),
          function(type, dataset, sspm_object, k, bs, xt, ...){

            # Get args from ellipsis for extra args: this form is necessary for
            # capturing symbols as well
            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            args_and_vars <- do.call(dispatch_smooth(type),
                                     append(list(sspm_object = sspm_object,
                                                 dataset = dataset,
                                                 dimension = "time",
                                                 k = k, bs = bs, xt = xt),
                                            args_list))

            # Assemble the smooths
            string_smooth <- assemble_smooth("s", args_and_vars$args)

            return(list(smooth = string_smooth,
                        vars = args_and_vars$vars))

          }
)

#' @export
#' @describeIn smooths TODO
setMethod(f = "smooth_space",
          signature(type = "ANY",
                    dataset = "character",
                    sspm_object = "sspm_discrete"),
          function(type, dataset, sspm_object, k, bs, xt, ...){

            # Get args from ellipsis for extra args: this form is necessary for
            # capturing symbols as well
            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            args_and_vars <- do.call(dispatch_smooth(type),
                                     append(list(sspm_object = sspm_object,
                                                 dataset = dataset,
                                                 dimension = "space",
                                                 k = k, bs = bs, xt = xt),
                                            args_list))

            # Assemble the smooths
            string_smooth <- assemble_smooth("s", args_and_vars$args)

            return(list(smooth = string_smooth,
                        vars = args_and_vars$vars))

          }
)

#' @export
#' @describeIn smooths TODO
setMethod(f = "smooth_space_time",
          signature(type = "ANY",
                    dataset = "character",
                    sspm_object = "sspm_discrete"),
          function(type, dataset, sspm_object, k, bs, xt, ...){

            # Get args from ellipsis for extra args: this form is necessary for
            # capturing symbols as well
            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            args_and_vars <- do.call(dispatch_smooth(type),
                                     append(list(sspm_object = sspm_object,
                                                 dataset = dataset,
                                                 dimension = "space_time",
                                                 k = k, bs = bs, xt = xt),
                                            args_list))

            # Assemble the smooths
            string_smooth <- assemble_smooth("ti", args_and_vars$args)

            return(list(smooth = string_smooth,
                        vars = args_and_vars$vars))

          }
)

# Accessory functions -----------------------------------------------------

# This functions turns the args_and_vars returned by ICAR (and potentially any
# any other functions like ICAR) into a call to a smooth (s, ti, etc...)
assemble_smooth <- function(s_type, args){

  checkmate::assert_character(s_type)
  checkmate::assert_list(args)

  deparse(rlang::call2(s_type, !!!args),
          width.cutoff = 500, nlines = 1)
}

# Construct an ICAR penalization matrix for a given "dimension" and returns the
# double list args_and_vars that have the args to build a new call to s() and the
# vars necessary for the evaluation of that s() smooth
ICAR <- function(sspm_object, dataset, dimension,
                 k, bs, xt, ...){

  checkmate::assert_class(sspm_object, "sspm")
  checkmate::assert_character(dataset)
  checkmate::assert_character(dimension)

  # Recapture the ellipsis again
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

  # Get data/dataset and relevant columns
  the_dataset <- spm_datasets(sspm_object)[[dataset]]
  the_data <- spm_data(the_dataset)

  # ---- TIME ----
  time_column <- spm_time_col(the_dataset)
  time_levels <- unique(the_data[[time_column]])
  n_time_levels = length(time_levels)

  # ---- SPACE ----
  # Here we assume the hardcoded convention that the patch column is patch_id
  # (from the discretization)
  space_column <- "patch_id"
  patches <- spm_patches(sspm_object)

  vars <- list()

  if (dimension == "time") {

    out_column <- list(str2lang(time_column))

    if(is.null(k)){
      k <- n_time_levels
    }

    if(is.null(xt)){
      pen_mat_time <- ICAR_time(time_levels, n_time_levels)
    } else {

      checkmate::assert_list(xt)

      if (is.null(xt$penalty)){
        pen_mat_time <- ICAR_time(time_levels, n_time_levels)
      } else {
        checkmate::assert_matrix(xt$penalty)
        pen_mat_time <- xt
      }
    }

    # Create symbol and assign to list
    pen_expression <- rlang::expr(pen_mat_time)
    vars$pen_mat_time <- pen_mat_time
    xt_list <- list(xt = list(penalty = pen_expression))

  } else if (dimension == "space"){

    out_column <- list(str2lang(space_column))

    if(is.null(xt)){
      pen_mat_space <- ICAR_space(patches, space_column)
    } else {

      checkmate::assert_list(xt)

      if (is.null(xt$penalty)){
        pen_mat_space <- ICAR_space(patches, space_column)
      } else {
        checkmate::assert_matrix(xt$penalty)
        pen_mat_space <- xt
      }
    }

    # Create symbol and assign to list
    pen_expression <- rlang::expr(pen_mat_space)
    vars$pen_mat_space <- pen_mat_space
    xt_list <- list(xt = list(penalty = pen_expression))

  } else if (dimension == "space_time"){

    out_column <- list(str2lang(time_column), str2lang(space_column))

    if(is.null(k)){
      k <- c(n_time_levels, 30)
    }

    if(is.null(xt)){

      pen_mat_time <- ICAR_time(time_levels, n_time_levels)
      pen_mat_space <- ICAR_space(patches, space_column)

      vars$pen_mat_time <- pen_mat_time
      vars$pen_mat_space <- pen_mat_space

    } else {

      checkmate::assert_list(xt)
      checkmate::assert_names(names(xt),
                              must.include = c(time_column, space_column))

      if (is.null(xt[[time_column]]$penalty)){
        vars$pen_mat_time <- ICAR_time(time_levels, n_time_levels)
      } else {
        vars$pen_mat_time <- xt[[time_column]]$penalty
      }

      if (is.null(xt[[space_column]]$penalty)){
        vars$pen_mat_space <- ICAR_space(patches, space_column)
      } else{
        vars$pen_mat_space <- xt[[space_column]]$penalty
      }

    }

    xt_list <- list(xt = list(list(penalty = rlang::expr(pen_mat_time)),
                              list(penalty = rlang::expr(pen_mat_space))))
    names(xt_list$xt) <- c(time_column, space_column)

  }

  return(list(args = do.call(c,
                             args = list(out_column,
                                         list(k=k, bs=bs),
                                         xt_list,
                                         args_list)),
              vars = vars))
}

ICAR_time <- function(time_levels, n_time_levels){

  # Creating an auto-regressive year penalty; this matrix means that the
  # estimate for each year is penalized to be close to the years before and
  # after it

  pen_mat = matrix(0, nrow=n_time_levels, ncol = n_time_levels)
  dimnames(pen_mat) = list(time_levels, time_levels)
  diag(pen_mat[-1,-n_time_levels]) = diag(pen_mat[-n_time_levels,-1]) = -1
  diag(pen_mat) = -(colSums(pen_mat)-diag(pen_mat))

  return(pen_mat)

}

ICAR_space <- function(patches, space_column){

  patches_adj_mat = suppressAll(sf::st_intersects(patches, sparse = FALSE))
  rownames(patches_adj_mat) = colnames(patches_adj_mat) = patches[[space_column]]
  patches_adj_mat = patches_adj_mat + 0
  diag(patches_adj_mat) = 0
  pen_mat = diag(rowSums(patches_adj_mat)) - patches_adj_mat

  return(pen_mat)

}