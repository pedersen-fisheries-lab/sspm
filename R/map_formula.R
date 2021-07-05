#' Map model formula onto a sspm_dataset object
#'
#' This functions is now used internally to map a formula onto a `sspm_dataset`
#' or `sspm` object.
#'
#' @inheritParams smooth_time
#' @inheritParams spm_smooth
#'
#' @return
#' The updated object.
#'
setGeneric(name = "map_formula",
           def = function(data_frame,
                          boundaries,
                          formula,
                          time_column,
                          ...) {
             standardGeneric("map_formula")
           }
)

## IMPORTANT NOTES:
# When gam or bam evaluate the formula, it seems that it starts by looking for
# variables in the s() statement in the parent environment and it will looks for
# response/predictors in the environment provided with the data=argument.
# Therefore all cannot be enclosed in the environment captured by the formula.

# Methods -----------------------------------------------------------------

#' @rdname map_formula
setMethod(f = "map_formula",
          signature(data_frame = "sf",
                    boundaries = "ANY",
                    formula = "formula"),
          function(data_frame, boundaries, formula, time_column, ...) {

            # Retrieve terms, response, and term labels
            formula_terms <- terms(formula)
            response <- all.vars(formula)[1]
            terms_labels <- attr(formula_terms, "term.labels")

            # Check response
            if (!checkmate::test_subset(response, names(data_frame))) {
              stop("The response in the formula is not a column of the dataset.",
                   call. = FALSE)
            }

            # Find the special calls to edit and evaluate
            is_special <- sapply(terms_labels, grepl, pattern = "smooth_lag(", fixed = TRUE) |
              sapply(terms_labels, grepl, pattern = "smooth_time(", fixed = TRUE) |
              sapply(terms_labels, grepl, pattern = "smooth_space(", fixed = TRUE) |
              sapply(terms_labels, grepl, pattern = "smooth_space_time(", fixed = TRUE)

            smooth_terms_labels <- terms_labels[is_special]
            other_terms <- terms_labels[!is_special]

            # Reconstruct base formula
            base_formula <- (paste(response, "~", paste(other_terms, collapse = " + "),
                                   collapse = " "))

            if (length(other_terms) > 0 & length(smooth_terms_labels) > 0) {
              base_formula <- paste0(base_formula, " + ")
            }

            # Capture calls and modify them
            smooth_calls <- lapply(smooth_terms_labels, str2lang)

            # Modify and evaluate the calls to get the args to make a smooth
            # TODO might be anle to simplify this
            smooth_calls_modified <-
              lapply(smooth_calls, modify_call,
                     args = list(data_frame = data_frame,
                                 boundaries = substitute(boundaries),
                                 time_column = time_column))
            smooth_and_vars <- lapply(smooth_calls_modified, eval,
                                     envir = list(. = data_frame,
                                                  boundaries = boundaries))

            smooth_list <- sapply(smooth_and_vars, `[[`, "smooth")

            vars_list <- purrr::flatten(lapply(smooth_and_vars, `[[`, "vars"))
            vars_list <- vars_list[unique(names(vars_list))]

            # Paste them into formula
            final_formula <- paste0(base_formula,
                                    paste(smooth_list, collapse = " + "))

            # Cast as formula
            # form_env <- list2env()
            final_formula_casted <- as.formula(final_formula)

            # Create new sspm_formula object
            sspm_formula <- new("sspm_formula",
                                raw_formula = formula,
                                translated_formula = final_formula_casted,
                                vars = vars_list,
                                type = "smooth",
                                response = response,
                                is_fitted = FALSE)

            return(sspm_formula)

          }
)
