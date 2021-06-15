#' Map model formula onto a sspm_data object
#'
#' This functions is now used internally to map a formula onto a `sspm_data`
#' or `sspm` object.
#'
#' @inheritParams spm_smooth
#'
#' @return
#' The updated object.
#'
setGeneric(name = "map_formula",
           def = function(sspm_object,
                          boundaries,
                          formula,
                          time_column,
                          ...){
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
          signature(sspm_object = "sspm_data",
                    boundaries = "sspm_discrete_boundary",
                    formula = "formula"),
          function(sspm_object, boundaries, formula, ...){

            browser()

            # This maps formula to a given dataset
            data_frame <- spm_data(sspm_object)
            time_column <- spm_time_column(sspm_object)

            sspm_formula <- map_formula(sspm_object = data_frame,
                                        boundaries = boundaries,
                                        formula = formula,
                                        time_column = time_column,
                                        ...)

            spm_formulas(sspm_object) <- append(spm_formulas(sspm_object),
                                                list(sspm_formula))

            return(sspm_object)

          }
)

#' @rdname map_formula
setMethod(f = "map_formula",
          signature(sspm_object = "sspm",
                    boundaries = "missing",
                    formula = "formula"),
          function(sspm_object, boundaries, formula, ...){

            browser()

            # If dataset name is not provided, assume we want to map an actual
            # SPM formula and not smooth the data (previous step in workflow)

            # 1. Is there a splitting scheme?
            if(!is_split(smoothed_data)){
              stop("Data must be split with a test/train column.")
            } else {
              old_data <- spm_smoothed_data(sspm_object)
              train_data <- old_data %>%
                dplyr::filter(.data$train_test == TRUE)
              spm_smoothed_data(sspm_object) <- train_data
            }

            data_frame <- spm_smoothed_data(sspm_object)
            time_column <- spm_time_column(sspm_object)

            # Pass onto the sspm_data method
            the_formula <- map_formula(sspm_object = data_frame,
                                       boundaries = boundaries,
                                       formula = formula,
                                       time_column = time_column,
                                       dataset = smoothed_data,
                                       ...)

            spm_smoothed_data(sspm_object) <- old_data

            return(sspm_object = sspm_object,
                   formula = the_formula)

          }
)

#' @rdname map_formula
setMethod(f = "map_formula",
          signature(sspm_object = "sf",
                    boundaries = "ANY",
                    formula = "formula"),
          function(sspm_object, boundaries, formula, time_column, ...){

            browser()

            # The object is now the data
            the_data <- sspm_object

            # Retrieve terms, response, and term labels
            formula_terms <- terms(formula)
            response <- all.vars(formula)[1]
            terms_labels <- attr(formula_terms, "term.labels")

            # Check response
            if(!checkmate::test_subset(response, names(the_data))){
              stop("The response in the formula is not a column of the dataset.",
                   call. = FALSE)
            }

            # Find the special calls to edit and evaluate
            is_special <- sapply(terms_labels, grepl, pattern="smooth_lag(", fixed=TRUE) |
              sapply(terms_labels, grepl, pattern="smooth_time(", fixed=TRUE) |
              sapply(terms_labels, grepl, pattern="smooth_space(", fixed=TRUE) |
              sapply(terms_labels, grepl, pattern="smooth_space_time(", fixed=TRUE)

            smooth_terms_labels <- terms_labels[is_special]
            other_terms <- terms_labels[!is_special]

            # Reconstruct base formula
            base_formula <- (paste(response, "~", paste(other_terms, collapse = " + "),
                                   collapse = " "))

            if(length(other_terms)>0) {
              base_formula <- paste0(base_formula, " + ")
            }

            # Capture calls and modify them
            smooth_calls <- lapply(smooth_terms_labels, str2lang)

            # Modify and evaluate the calls to get the args to make a smooth
            smooth_calls_modified <-
              lapply(smooth_calls, modify_call,
                     args = list(data_frame = sspm_object,
                                 boundaries = substitute(boundaries),
                                 time_column = time_column))
            smooth_and_vars <-lapply(smooth_calls_modified, eval,
                                     envir = list(. = sspm_object,
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
