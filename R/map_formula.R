#' Map model formula onto a discretized SPASPM object
#'
#' This functions allows to specify a model formula for a given discrete SPASPM
#' object. This formula makes use of specific smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`. This formula can also contain fixed
#' effects and custom smooths, and can make use of specific smoothing terms
#' `smooth_time()`, `smooth_space()`, `smooth_space_time()`. See Details for
#' more explanations.
#'
#' @param spaspm_object **\[spaspm_discrete\]** An object of class
#'     [spaspm_discrete][spaspm_discrete-class].
#' @param dataset **\[character\]** The name of the dataset among base and/or
#'     mapped datasets for which to specify the formula
#' @param formula **\[formula\]** A formula definition of the form
#'     response ~ smoothing_terms + ...
#' @param ... Further arguments passed down, none used for now.
#'
#' @return
#' The updated object, of class [spaspm_discrete][spaspm_discrete-class].
#'
#' @export
setGeneric(name = "map_formula",
           def = function(spaspm_object,
                          dataset,
                          formula,
                          ...){

             all_dataset_names <- names(spm_datasets(spaspm_object))
             if(!checkmate::test_choice(dataset, all_dataset_names)){
               stop(paste0("Argument 'dataset' must be one of: ",
                           paste0(all_dataset_names,
                                  collapse =  ", " )), call. = FALSE)
             }

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
#' @describeIn map_formula TODO
setMethod(f = "map_formula",
          signature(spaspm_object = "spaspm"),
          function(spaspm_object, ...){
            message_not_discrete(spaspm_object)
          }
)

#' @export
#' @describeIn map_formula TODO
setMethod(f = "map_formula",
          signature(spaspm_object = "spaspm_discrete",
                    dataset = "missing"),
          function(spaspm_object, dataset, formula, ... ){
            cli::cli_alert_danger(" Argument 'dataset' missing with no default")
          }
)

#' @export
#' @describeIn map_formula TODO
setMethod(f = "map_formula",
          signature(spaspm_object = "spaspm_discrete",
                    formula = "missing"),
          function(spaspm_object, dataset, formula, ... ){
            cli::cli_alert_danger(" Argument 'formula' missing with no default")
          }
)

#' @export
#' @describeIn map_formula TODO
setMethod(f = "map_formula",
          signature(spaspm_object = "spaspm_discrete",
                    dataset = "character",
                    formula = "formula"),
          function(spaspm_object, dataset, formula, ...){

            # Retrieve terms, response, and term labels
            formula_terms <- terms(formula)
            response <- all.vars(formula)[1]
            terms_labels <- attr(formula_terms, "term.labels")

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
                     args = list(spaspm_object = substitute(spaspm_object),
                                 dataset = substitute(dataset)))

            # Evaluate the calls to get the args to make a smooth
            smooth_and_vars <-lapply(smooth_calls_modified, eval,
                                   envir = list(. = spaspm_object))

            smooth_list <- sapply(smooth_and_vars, `[[`, "smooth")

            vars_list <- purrr::flatten(lapply(smooth_and_vars, `[[`, "vars"))
            # if(purrr::vec_depth(vars_list)>2){
            #   vars_list <- purrr::flatten(vars_list)
            # }
            vars_list <- vars_list[unique(names(vars_list))]

            # Paste them into formula
            final_formula <- paste0(base_formula,
                                    paste(smooth_list, collapse = " + "))

            # Cast as formula
            # form_env <- list2env()
            final_formula_casted <- as.formula(final_formula)

            # Create new spaspm_formula object
            spaspm_formula <- new("spaspm_formula",
                                  raw_formula = formula,
                                  translated_formula = final_formula_casted,
                                  dataset = dataset,
                                  vars = vars_list)

            spm_mapped_formulas(spaspm_object) <-
              append(spm_mapped_formulas(spaspm_object),
                     list(spaspm_formula))

            return(spaspm_object)
          }
)

# -------------------------------------------------------------------------
