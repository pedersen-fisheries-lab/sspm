#' sspm Smoothing functions
#'
#' A full sspm formula contains calls to the smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`.
#'
#' @param data_frame **\[sf data.frame\]** The data.
#' @param time_column **\[character\]** The time column.
#' @param var **\[symbol\]** Variable (only for smooth_lag).
#' @param type **\[character\]** Type of smooth, currently only "ICAR" is
#'     supported.
#' @param k **\[numeric\]** Size of the smooths and/or size of the lag.
#' @inheritParams spm_smooth
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
           def = function(data_frame,
                          boundaries,
                          time_column,
                          type = "ICAR",
                          k = NULL,
                          bs = "re",
                          xt = NULL,
                          ...){
             standardGeneric("smooth_time")
           }
)

#' @export
#' @rdname smooths
setGeneric(name = "smooth_space",
           def = function(data_frame,
                          boundaries,
                          time_column,
                          type = "ICAR",
                          k = 30,
                          bs = "mrf",
                          xt = NULL,
                          ...){
             standardGeneric("smooth_space")
           }
)

#' @export
#' @rdname smooths
setGeneric(name = "smooth_space_time",
           def = function(data_frame,
                          boundaries,
                          time_column,
                          type = "ICAR",
                          k = NULL,
                          bs = c("re","mrf"),
                          xt = NULL,
                          ...){
             standardGeneric("smooth_space_time")
           }
)

#' @export
#' @rdname smooths
setGeneric(name = "smooth_lag",
           def = function(var,
                          data_frame,
                          boundaries,
                          time_column,
                          type = "LINPRED",
                          k = 5,
                          m = 1,
                          ...){
             standardGeneric("smooth_lag")
           }
)

# Methods -----------------------------------------------------------------

# Beware: the following code is repeated three time for the sake of user
# friendliness, although much has been done to limit repetition. If you are
# about to edit some of this code, check whether this edit should not be also
# done to the other two occurrences of that code.

#' @export
#' @rdname smooths
setMethod(f = "smooth_time",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time_column, type, k, bs, xt, ...){

            # Get args from ellipsis for extra args: this form is necessary for
            # capturing symbols as well
            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            args_and_vars <- do.call(dispatch_smooth(type),
                                     append(list(data_frame = data_frame,
                                                 time_column = time_column,
                                                 dimension = "time",
                                                 boundaries = boundaries,
                                                 k = k, bs = bs, xt = xt),
                                            args_list))

            # Assemble the smooths
            string_smooth <- assemble_smooth("s", args_and_vars$args)

            return(list(smooth = string_smooth,
                        vars = args_and_vars$vars))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_space",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time_column, type, k, bs, xt, ...){

            # Get args from ellipsis for extra args: this form is necessary for
            # capturing symbols as well
            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            args_and_vars <- do.call(dispatch_smooth(type),
                                     append(list(data_frame = data_frame,
                                                 time_column = time_column,
                                                 dimension = "space",
                                                 boundaries = boundaries,
                                                 k = k, bs = bs, xt = xt),
                                            args_list))

            # Assemble the smooths
            string_smooth <- assemble_smooth("s", args_and_vars$args)

            return(list(smooth = string_smooth,
                        vars = args_and_vars$vars))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_space_time",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time_column, type, k, bs, xt, ...){

            # Get args from ellipsis for extra args: this form is necessary for
            # capturing symbols as well
            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            args_and_vars <- do.call(dispatch_smooth(type),
                                     append(list(data_frame = data_frame,
                                                 time_column = time_column,
                                                 dimension = "space_time",
                                                 boundaries = boundaries,
                                                 k = k, bs = bs, xt = xt),
                                            args_list))

            # Assemble the smooths
            string_smooth <- assemble_smooth("ti", args_and_vars$args)

            return(list(smooth = string_smooth,
                        vars = args_and_vars$vars))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_lag",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(var, data_frame, boundaries, time_column, type, k, m, ...){
            # Get args from ellipsis for extra args: this form is necessary for
            # capturing symbols as well
            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            # Get the default arguments for the smooth type used
            args_and_vars <- do.call(dispatch_smooth(type),
                                     append(list(data_frame = data_frame,
                                                 boundaries = boundaries,
                                                 time_column = time_column,
                                                 var = var,
                                                 k = k, m = m),
                                            args_list))

            # Assemble the smooths
            string_smooth <- assemble_smooth("s", args_and_vars$args)

            return(list(smooth = string_smooth,
                        vars = args_and_vars$vars))

          }
)

# ICAR --------------------------------------------------------------------

# Construct an ICAR penalization matrix for a given "dimension" and returns the
# double list args_and_vars that have the args to build a new call to s() and the
# vars necessary for the evaluation of that s() smooth
ICAR <- function(data_frame, boundaries, time_column, dimension,
                 k, bs, xt, ...){

  checkmate::assert_class(data_frame, "sf")
  checkmate::assert_class(boundaries, "sspm_discrete_boundary")
  checkmate::assert_character(time_column)
  checkmate::assert_character(dimension)
  checkmate::assert_choice(dimension, choices = c("time", "space", "space_time"))

  # Recapture the ellipsis again
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

  # ---- TIME ----
  time_levels <- unique(data_frame[[time_column]])
  n_time_levels = length(time_levels)

  # ---- SPACE ----
  # Here we assume the hardcoded convention that the patch column is patch_id
  # (from the discretization)
  space_column <- "patch_id"
  patches <- boundaries@patches

  # Setup done ----

  vars <- list()

  if (dimension == "time") {

    out_column <- list(str2lang(time_column))

    if(is.null(k)){
      k <- n_time_levels
    }

    if(is.null(bs)){
      bs <- "re"
    }

    if(is.null(xt)){
      pen_mat_time <- ICAR_time(time_levels)
    } else {

      checkmate::assert_list(xt)

      if (is.null(xt$penalty)){
        pen_mat_time <- ICAR_time(time_levels)
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

    if(is.null(k)){
      k <- 30
    }

    if(is.null(bs)){
      bs <- "mrf"
    }

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

    if(is.null(bs)){
      bs <- c("re","mrf")
    }

    if(is.null(xt)){

      pen_mat_time <- ICAR_time(time_levels)
      pen_mat_space <- ICAR_space(patches, space_column)

      vars$pen_mat_time <- pen_mat_time
      vars$pen_mat_space <- pen_mat_space

    } else {

      # Must be a list of list with correct names
      checkmate::assert_list(xt)
      lapply(xt, checkmate::assert_list)
      checkmate::assert_names(names(xt),
                              subset.of = c(time_column, space_column))

      if (is.null(xt[[time_column]]$penalty)){
        vars$pen_mat_time <- ICAR_time(time_levels)
      } else {
        checkmate::assert_matrix(xt[[time_column]]$penalty)
        vars$pen_mat_time <- xt[[time_column]]$penalty
      }

      if (is.null(xt[[space_column]]$penalty)){
        vars$pen_mat_space <- ICAR_space(patches, space_column)
      } else{
        checkmate::assert_matrix(xt[[space_column]]$penalty)
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

ICAR_time <- function(time_levels){

  # Creating an auto-regressive year penalty; this matrix means that the
  # estimate for each year is penalized to be close to the years before and
  # after it

  time_levels <- sort(time_levels)
  n_time_levels <- length(unique(time_levels))

  pen_mat = matrix(0, nrow=n_time_levels, ncol = n_time_levels)
  dimnames(pen_mat) = list(time_levels, time_levels)
  diag(pen_mat[-1,-n_time_levels]) = diag(pen_mat[-n_time_levels,-1]) = -1
  diag(pen_mat) = -(colSums(pen_mat)-diag(pen_mat))

  return(pen_mat)

}

ICAR_space <- function(patches, space_column){

  checkmate::assert_choice(space_column, names(patches))

  patches_adj_mat = suppressAll(sf::st_intersects(patches, sparse = FALSE))
  dimnames(patches_adj_mat) = list(unique(patches[[space_column]]),
                                   unique(patches[[space_column]]))
  patches_adj_mat = patches_adj_mat + 0
  diag(patches_adj_mat) = 0
  pen_mat = diag(rowSums(patches_adj_mat)) - patches_adj_mat

  return(pen_mat)

}

# LINPRED -----------------------------------------------------------------

# Construct the lag matrix and associated lag columns for the linear predictor
# method of fitting the smooth

LINPRED <- function(data_frame, boundaries, time_column, var,
                    k, m, ...){

  checkmate::assert_class(data_frame, "sf")
  checkmate::assert_class(boundaries, "sspm_discrete_boundary")
  checkmate::assert_character(time_column)

  # Recapture the ellipsis again
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

  # Make the lag matrix
  boundary_col <- spm_boundary_colum(boundaries)

  lag_matrix <- as.data.frame(matrix(-(1:k), nrow = nrow(data_frame),
                                     ncol = k, byrow = TRUE)) %>%
    dplyr::rename_all(.funs = gsub, pattern = "V", replacement = "lag") %>%
    dplyr::mutate(!!time_column := data_frame[[time_column]],
                  !!boundary_col := data_frame[[boundary_col]],
                  "patch_id" = data_frame[["patch_id"]]) %>%
    dplyr:: select(dplyr::contains('lag')) %>%
    as.matrix()

  by_matrix <- data_frame %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(.data$patch_id, !!boundary_col, !!time_column, !!var) %>%
    dplyr::nest_by(.data$patch_id, !!boundary_col := .data[[boundary_col]]) %>%
    dplyr::mutate(lags = list(multilag(variable = .data$data[[var]],
                                       n_lags = k,
                                       # TODO: assuming in-group mean as default
                                       default = mean(.data$data[[var]],
                                                      na.rm = T)))) %>%
    tidyr::unnest(cols = c(.data$lags, .data$data)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::contains('lag')) %>%
    dplyr::select(-dplyr::contains(var)) %>%
    as.matrix()

  out_column <- list(str2lang("lag_matrix"))
  vars <- list()
  vars$lag_matrix <- lag_matrix
  vars$by_matrix <- by_matrix

  return(list(args = do.call(c,
                             args = list(out_column,
                                         list(k=k, m=m,
                                              by=str2lang("by_matrix")),
                                         args_list)),
              vars = vars))

}

# Accessory functions -----------------------------------------------------

# This functions turns the args_and_vars returned by ICAR (and potentially any
# any other functions like ICAR) into a call to a smooth (s, ti, etc...)
assemble_smooth <- function(s_type, args){

  checkmate::assert_character(s_type)
  checkmate::assert_list(args)

  deparse(rlang::call2(s_type, !!!args),
          width.cutoff = 500, nlines = 1)
}
