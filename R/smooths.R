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
#' @param is_spm Whether or not an SPM is being fitted (used internally)
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
                          is_spm = FALSE,
                          ...) {
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
                          k = NULL,
                          bs = "mrf",
                          xt = NULL,
                          is_spm = FALSE,
                          ...) {
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
                          bs = c("re", "mrf"),
                          xt = NULL,
                          is_spm = FALSE,
                          ...) {
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
                          ...) {
             standardGeneric("smooth_lag")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname smooths
setMethod(f = "smooth_time",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time_column, type, k, bs, xt, is_spm, ...) {

            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            do.call(smooth_routine,
                    append(list(dimension = "time", var = NULL, data_frame = data_frame,
                                boundaries = boundaries, time_column = time_column,
                                type = type, k = k, m = NULL, bs = bs, xt = xt,
                                is_spm = is_spm, smooth_type = "s"),
                           args_list))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_space",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time_column, type, k, bs, xt, is_spm, ...) {

            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            do.call(smooth_routine,
                    append(list(dimension = "space", var = NULL, data_frame = data_frame,
                                boundaries = boundaries, time_column = time_column,
                                type = type, k = k, m = NULL, bs = bs, xt = xt,
                                is_spm = is_spm, smooth_type = "s"),
                           args_list))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_space_time",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time_column, type, k, bs, xt, is_spm, ...) {

            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            do.call(smooth_routine,
                    append(list(dimension = "space_time", var = NULL, data_frame = data_frame,
                                boundaries = boundaries, time_column = time_column,
                                type = type, k = k, m = NULL, bs = bs, xt = xt,
                                is_spm = is_spm, smooth_type = "ti"),
                           args_list))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_lag",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(var, data_frame, boundaries, time_column, type, k, m, ...) {

            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            do.call(smooth_routine,
                    append(list(dimension = NULL, var = var, data_frame = data_frame,
                                boundaries = boundaries, time_column = time_column,
                                type = type, k = k, m = m, bs = NULL, xt = NULL,
                                is_spm = NULL, smooth_type = "s"),
                           args_list))

          }
)


# Routine -----------------------------------------------------------------

smooth_routine <- function(dimension, var, data_frame, boundaries, time_column,
                           type, k, m, bs, xt, is_spm, smooth_type, ...){

  # Get args from ellipsis for extra args: this form is necessary for
  # capturing symbols as well
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

  # Get the default arguments for the smooth type used
  args_and_vars <- do.call(dispatch_smooth(type),
                           append(list(dimension = dimension,
                                       var = var,
                                       data_frame = data_frame,
                                       boundaries = boundaries,
                                       time_column = time_column,
                                       k = k, m = m,
                                       bs = bs, xt = xt,
                                       is_spm = is_spm),
                                  args_list))

  # Assemble the smooths
  string_smooth <- assemble_smooth(smooth_type, args_and_vars$args)
  ret_list <- list(smooth = string_smooth,
                   vars = args_and_vars$vars)

  if (!is.null(var)){
    ret_list$var_smooth_lag <- var
  } else {
    ret_list$var_smooth_lag <- NULL
  }

  return(ret_list)

}

# ICAR --------------------------------------------------------------------

# Construct an ICAR penalization matrix for a given "dimension" and returns the
# double list args_and_vars that have the args to build a new call to s() and the
# vars necessary for the evaluation of that s() smooth
ICAR <- function(data_frame, boundaries, time_column, dimension,
                 k, bs, xt, is_spm, unused_names = c("var", "m"), ...) {

  checkmate::assert_class(data_frame, "sf")
  checkmate::assert_class(boundaries, "sspm_discrete_boundary")
  checkmate::assert_character(time_column)
  checkmate::assert_character(dimension)
  checkmate::assert_choice(dimension, choices = c("time", "space", "space_time"))

  # Recapture the ellipsis again
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)
  args_list <- args_list[!(names(args_list) %in% unused_names)]

  # ---- TIME ----
  time_levels <- levels(data_frame[[time_column]])
  n_time_levels = as.numeric(length(time_levels))

  # ---- SPACE ----
  # Here we assume the hardcoded convention that the patch column is patch_id
  # (from the discretization)
  space_column <- "patch_id"
  patches <- boundaries@patches

  # Setup done ----
  vars <- list()

  if (dimension == "time") {

    out_column <- list(str2lang(time_column))

    if (is.null(k)) {
      if (!is_spm) {
        k <- n_time_levels
      }
    }

    if (is.null(bs)) {

      # If no bs specified, go with re, no penalty needed
      bs <- "re"

      xt_list <- NULL

    } else if (bs == "re"){

      xt_list <- NULL

    } else if (bs == "mrf"){ # If mrf specified, provide the matrix

      if (is.null(xt)) {

        pen_mat_time <- ICAR_time(time_levels)

      } else {

        checkmate::assert_list(xt)

        if (is.null(xt$penalty)) {
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

    }

  } else if (dimension == "space") {

    out_column <- list(str2lang(space_column))

    if (is.null(k)) {
      if (!is_spm) {
        k <- 30
      }
    }

    if (is.null(bs)) {
      bs <- "mrf"
    }

    if (is.null(xt)) {

      pen_mat_space <- ICAR_space(patches, space_column)

    } else {

      checkmate::assert_list(xt)

      if (is.null(xt$penalty)) {
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

  } else if (dimension == "space_time") {

    out_column <- list(str2lang(time_column), str2lang(space_column))

    if (is.null(k)) {
      if (is_spm) {
        k <- c(NA, 30)
      } else {
        k <- c(n_time_levels, 30)
      }
    }

    if (is.null(bs)) {

      bs <- c("re", "mrf")

      xt_list <- NULL

    } else if (identical(bs, c("mrf", "mrf"))){ # If mrf specified, provide the matrix

      if (is.null(xt)) {

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

        if (is.null(xt[[time_column]]$penalty)) {
          vars$pen_mat_time <- ICAR_time(time_levels)
        } else {
          checkmate::assert_matrix(xt[[time_column]]$penalty)
          vars$pen_mat_time <- xt[[time_column]]$penalty
        }

        if (is.null(xt[[space_column]]$penalty)) {
          vars$pen_mat_space <- ICAR_space(patches, space_column)
        } else {
          checkmate::assert_matrix(xt[[space_column]]$penalty)
          vars$pen_mat_space <- xt[[space_column]]$penalty
        }

      }

      xt_list <- list(xt = list(list(penalty = rlang::expr(pen_mat_time)),
                                list(penalty = rlang::expr(pen_mat_space))))
      names(xt_list$xt) <- c(time_column, space_column)

    }

    if (is.null(xt)) {

      pen_mat_space <- ICAR_space(patches, space_column)

      vars$pen_mat_space <- pen_mat_space

    } else {

      # Must be a list of list with correct names
      checkmate::assert_list(xt)
      lapply(xt, checkmate::assert_list)
      checkmate::assert_names(names(xt),
                              subset.of = c(time_column, space_column))

      if (is.null(xt[[space_column]]$penalty)) {
        vars$pen_mat_space <- ICAR_space(patches, space_column)
      } else {
        checkmate::assert_matrix(xt[[space_column]]$penalty)
        vars$pen_mat_space <- xt[[space_column]]$penalty
      }

    }

    # Create symbol and assign to list
    pen_expression <- rlang::expr(pen_mat_space)
    vars$pen_mat_space <- pen_mat_space
    xt_list <- list(xt = list(penalty = pen_expression))

  }

  return(list(args = do.call(c,
                             args = list(out_column,
                                         list(k = k, bs = bs),
                                         xt_list,
                                         args_list)),
              vars = vars))
}

ICAR_time <- function(time_levels) {

  # Creating an auto-regressive year penalty; this matrix means that the
  # estimate for each year is penalized to be close to the years before and
  # after it

  time_levels <- sort(time_levels)
  # time_levels <- 1979:2018

  n_time_levels <- length(unique(time_levels))

  pen_mat = matrix(0, nrow = n_time_levels, ncol = n_time_levels)
  dimnames(pen_mat) = list(time_levels, time_levels)
  diag(pen_mat[-1, -n_time_levels]) = diag(pen_mat[-n_time_levels, -1]) = -1
  diag(pen_mat) = -(colSums(pen_mat) - diag(pen_mat))

  return(pen_mat)

}

ICAR_space <- function(patches, space_column) {

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

LINPRED <- function(data_frame, boundaries, time_column, var, k, m,
                    unused_names = c("dimension", "bs", "xt", "is_spm"), ...) {

  checkmate::assert_class(data_frame, "sf")
  checkmate::assert_class(boundaries, "sspm_discrete_boundary")
  checkmate::assert_character(time_column)

  # Recapture the ellipsis again
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)
  args_list <- args_list[!(names(args_list) %in% unused_names)]

  # Make the lag matrix
  boundary_col <- spm_boundary_column(boundaries)

  lag_matrix <- as.data.frame(matrix(-(1:k), nrow = nrow(data_frame),
                                     ncol = k, byrow = TRUE)) %>%
    dplyr::rename_all(.funs = gsub, pattern = "V", replacement = "lag") %>%
    dplyr::mutate(!!time_column := data_frame[[time_column]],
                  !!boundary_col := data_frame[[boundary_col]],
                  "patch_id" = data_frame[["patch_id"]]) %>%
    dplyr::select(dplyr::contains('lag')) %>%
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
                                         list(k = k, m = m,
                                              by = str2lang("by_matrix")),
                                         args_list)),
              vars = vars))

}

# Accessory functions -----------------------------------------------------

# This functions turns the args_and_vars returned by ICAR (and potentially any
# any other functions like ICAR) into a call to a smooth (s, ti, etc...)
assemble_smooth <- function(s_type, args) {

  checkmate::assert_character(s_type)
  checkmate::assert_list(args)

  deparse(rlang::call2(s_type, !!!args),
          width.cutoff = 500, nlines = 1)
}
