#' Map model formula onto a discretized sspm object
#'
#' This functions allows to specify a model formula for a given discrete sspm
#' object. This formula makes use of specific smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`. This formula can also contain fixed
#' effects and custom smooths, and can make use of specific smoothing terms
#' `smooth_time()`, `smooth_space()`, `smooth_space_time()`. See Details for
#' more explanations.
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'     [sspm_discrete][sspm_discrete-class].
#' @param formula **\[formula\]** A formula definition of the form
#'     response ~ smoothing_terms + ...
#' @param dataset **\[character\]** The name(s) of the dataset(s) for which to
#'     specify the formula.
#' @param ... Further arguments passed down, none used for now.
#'
#' @return
#' The updated object, of class [sspm_discrete][sspm_discrete-class].
#'
#' @export
setGeneric(name = "map_formula",
           def = function(sspm_object,
                          formula,
                          dataset,
                          ...){
             standardGeneric("map_formula")
           }
)

## IMPORTANT NOTES:
# I have realized that when gam or bam evaluate the formula, it looks for
# variables in the s() statement in the parent environment and it will looks for
# response/predictors in the environment provided with the data=argument.
# Therefore all cannot be enclosed in the environment captured by the formula.

# Methods -----------------------------------------------------------------

#' @export
#' @rdname map_formula
setMethod(f = "map_formula",
          signature(sspm_object = "sspm_discrete",
                    formula = "missing"),
          function(sspm_object, formula, dataset, ... ){
            cli::cli_alert_danger(" Argument 'formula' missing with no default")
          }
)

#' @export
#' @rdname map_formula
setMethod(f = "map_formula",
          signature(sspm_object = "sspm_discrete",
                    formula = "formula",
                    dataset = "missing"),
          function(sspm_object, formula, dataset, ...){

            # If dataset name is not provided, assume we want to map an actual
            # SPM formula and not smooth the data (previous step in workflow)

            # So first determine whether all datasets have been smoothed and
            # whether a splitting scheme has been provided

            # Get all datasets
            all_datasets <- spm_datasets(sspm_object)
            smoothed_data <- spm_smoothed_data(sspm_object)

            # 1. Are all datasets smoothed?
            are_smoothed <- sapply(all_datasets, is_smoothed)

            if(!(any(are_smoothed))){
              cli::cli_alert_warning(" Warning: Not all datasets are smoothed")
              cli::cli_alert_info(" To fit a smoothing formula to a specific dataset, use dataset = ...")
              # stop("Not all datasets are smoothed", call. = FALSE)
            }

            # 2. Is there a dataset of type "biomass" and one of type "catch"
            # all_types <- sapply(all_datasets, spm_type)
            # TODO Uncomment this before next release
            # if(any(!("biomass" %in% all_types) | !("catch" %in% all_types))){
            #   cli::cli_alert_danger(" No dataset of type biomass or catch")
            #   stop("No dataset of type biomass or catch", call. = FALSE)
            # }

            # 3. Is there a splitting scheme?
            if(!is_splitted(smoothed_data)){
              stop("Data must be splitted.")
            } else {
              old_sspm_data <- spm_data(spm_smoothed_data(sspm_object))
              new_sspm_data <- old_sspm_data %>%
                dplyr::filter(.data$train_test == TRUE)
              spm_data(spm_smoothed_data(sspm_object)) <- new_sspm_data
            }

            # 4. Map the formula

            # Retrieve terms, response, and term labels
            formula_terms <- terms(formula)
            response <- all.vars(formula)[1]
            terms_labels <- attr(formula_terms, "term.labels")

            # Check response

            response_data <- spm_data(smoothed_data)

            if(!checkmate::test_subset(response, names(response_data))){
              stop("The response in the formula is not a column of the smoothed_data.",
                   call. = FALSE)
            }

            # Pass onto the sspm_data method
            smoothed_data <- sspm_object %>%
              map_formula(formula = formula,
                          dataset = smoothed_data,
                          ...)

            spm_smoothed_data(sspm_object) <- smoothed_data
            spm_data(spm_smoothed_data(sspm_object)) <- old_sspm_data

            return(sspm_object)

          }
)

#' @export
#' @rdname map_formula
setMethod(f = "map_formula",
          signature(sspm_object = "sspm_discrete",
                    formula = "formula",
                    dataset = "character"),
          function(sspm_object, formula, dataset, ...){

            # Get datasets
            all_datasets <- spm_datasets(sspm_object)

            # Check names
            all_dataset_names <- names(all_datasets)
            if(any(!sapply(dataset, checkmate::test_choice, all_dataset_names))){
              stop(paste0("Argument 'dataset' must be one of: ",
                          paste0(all_dataset_names,
                                 collapse =  ", " )), call. = FALSE)
            }

            # Process each dataset in a loop
            for (dataset_name in dataset){

              the_dataset <- spm_datasets(sspm_object)[[dataset_name]]

              if(is_smoothed(the_dataset)){
                cli::cli_alert_danger("Dataset is already smoothed.")
                stop(call. = FALSE)
              }

              # Pass onto the sspm_data method
              the_dataset <- sspm_object %>%
                map_formula(formula = formula,
                            dataset = the_dataset,
                            ...)

              all_datasets[[dataset_name]] <- the_dataset
              spm_datasets(sspm_object) <- all_datasets

            }

            return(sspm_object)

          }
)

#' @export
#' @rdname map_formula
setMethod(f = "map_formula",
          signature(sspm_object = "sspm_discrete",
                    formula = "formula",
                    dataset = "sspm_data"),
          function(sspm_object, formula, dataset, ...){
            # This maps foprmula to a given dataset

            dataset_name <- spm_name(dataset)

            # Retrieve terms, response, and term labels
            formula_terms <- terms(formula)
            response <- all.vars(formula)[1]
            terms_labels <- attr(formula_terms, "term.labels")

            # Check response
            the_data <- spm_data(dataset)
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

            smooth_calls_modified <-
              lapply(smooth_calls, modify_call,
                     args = list(sspm_object = substitute(sspm_object),
                                 dataset = substitute(dataset_name)))

            # Evaluate the calls to get the args to make a smooth
            smooth_and_vars <-lapply(smooth_calls_modified, eval,
                                     envir = list(. = sspm_object))

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
                                type = "smooth")

            spm_formulas(dataset) <- append(spm_formulas(dataset),
                                            list(sspm_formula))

            return(dataset)

          }
)

# -------------------------------------------------------------------------
