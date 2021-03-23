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
#' @param dataset **\[character\]** The name of the dataset among base and/or
#'     mapped datasets for which to specify the formula
#' @param formula **\[formula\]** A formula definition of the form
#'     response ~ smoothing_terms + ...
#' @param ... Further arguments passed down, none used for now.
#'
#' @return
#' The updated object, of class [sspm_discrete][sspm_discrete-class].
#'
#' @export
setGeneric(name = "map_formula",
           def = function(sspm_object,
                          dataset,
                          formula,
                          ...){
             standardGeneric("map_formula")
           }
)

## IMPORTANT NOTES:
# I have realized that when gam or bam evaluate the formula, it looks for
# variables in the s() statement in the parent environment and it will looks for
# response/predictors in the environment provided with the data=argument.
# Therefore all connot be enclosed in the environment captured by the
# formula

# Methods -----------------------------------------------------------------

#' @export
#' @rdname map_formula 
setMethod(f = "map_formula",
          signature(sspm_object = "sspm"),
          function(sspm_object, ...){
            message_not_discrete(sspm_object)
          }
)

#' @export
#' @rdname map_formula 
setMethod(f = "map_formula",
          signature(sspm_object = "sspm_discrete",
                    dataset = "missing"),
          function(sspm_object, dataset, formula, ... ){
            cli::cli_alert_danger(" Argument 'dataset' missing with no default")
          }
)

#' @export
#' @rdname map_formula 
setMethod(f = "map_formula",
          signature(sspm_object = "sspm_discrete",
                    formula = "missing"),
          function(sspm_object, dataset, formula, ... ){
            cli::cli_alert_danger(" Argument 'formula' missing with no default")
          }
)

#' @export
#' @rdname map_formula 
setMethod(f = "map_formula",
          signature(sspm_object = "sspm_discrete",
                    dataset = "character",
                    formula = "formula"),
          function(sspm_object, dataset, formula, ...){

            # Get datasets
            all_datasets <- spm_datasets(sspm_object)

            # Check names
            all_dataset_names <- names(all_datasets)
            if(!checkmate::test_choice(dataset, all_dataset_names)){
              stop(paste0("Argument 'dataset' must be one of: ",
                          paste0(all_dataset_names,
                                 collapse =  ", " )), call. = FALSE)
            }

            # Retrieve terms, response, and term labels
            formula_terms <- terms(formula)
            response <- all.vars(formula)[1]
            terms_labels <- attr(formula_terms, "term.labels")

            # Check response
            the_dataset <- all_datasets[[dataset]]

            if(the_dataset@is_smoothed){
              cli::cli_alert_danger("Dataset is already smoothed.")
              stop(call. = FALSE)
            }

            the_data <- spm_data(the_dataset)
            if(!checkmate::test_subset(response, names(the_data))){
              stop("The response in the formula is not a column of the dataset.",
                   call. = FALSE)
            }

            # Find the special calls to edit and evaluate
            is_special <- sapply(terms_labels, grepl, pattern="smooth_", fixed=TRUE)
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
                                 dataset = substitute(dataset)))

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

            spm_formulas(the_dataset) <- append(spm_formulas(the_dataset),
                                                list(sspm_formula))

            all_datasets[[dataset]] <- the_dataset
            spm_datasets(sspm_object) <- all_datasets

            return(sspm_object)
          }
)

# -------------------------------------------------------------------------
