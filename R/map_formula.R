#' Map model formula onto a discretized SPASPM object
#'
#' This functions allows to specify a model formula for a given discrete SPASPM
#' object. This formula makes use of specific smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`. This formula can also contain fixed
#' effects and custom smooths, and can make use of specific smoothing terms
#' `smooth_time()`, `smooth_space()`, `smooth_space_time()`. See Details for
#' more explanations.
#'
#'  @param spaspm_object **\[spaspm_discrete\]** An object of class
#'    [spaspm_discrete][spaspm_discrete-class].
#'  @param dataset **\[character\]** The name of the dataset among base and/or
#'    mapped datasets for which to specify the formula
#'  @param formula **\[formula\]** A formula definition of the form
#'    response ~ smoothing_terms + ...
#'  @param ... Further arguments passed down, none used for now.
#'
#' @export
setGeneric(name = "map_formula",
           def = function(spaspm_object,
                          dataset,
                          formula,
                          ...){
             standardGeneric("map_formula")
           }
)

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
          function(spaspm_object, dataset, formula, ... ){

            # retrieve terms, response, and term labels
            formula_terms <- terms(formula)
            response <- all.vars(formula)[1]
            terms_labels <- attr(formula_terms, "term.labels")

            # Find the special calls to edit and evaluate
            is_special <- sapply(terms_labels, grepl, pattern="smooth_", fixed=TRUE)
            smooth_terms_labels <- terms_labels[is_special]
            other_terms <- terms_labels[!is_special]

            # Reconstruct base formula
            base_formula <- (paste(response, "~", paste(other_terms, collapse = " + ")
                                   , collapse = " "))

            # Capture calls and modify them
            smooth_calls <- lapply(smooth_terms_labels, str2lang)

            smooth_calls_modified <-
              lapply(smooth_calls, modify_call,
                     args = list(spaspm_object = substitute(spaspm_object),
                                 dataset = substitute(dataset)))

            # Evaluate the calls to get the quoted smooths
            evaluated <- lapply(smooth_calls_modified, eval)
            evaluated_string <- sapply(evaluated, deparse)

            # Paste them into formula
            final_formula <- paste0(base_formula, " + ",
                                    paste(evaluated_string, collapse = " + "))

            # Cast as formula
            # TODO unsure if it's worth casting here
            # final_formula_casted <- as.formula(final_formula)

            # Returns an updated spaspm object WITH mapped formula
            # TODO add the formula to dataset (we know which from argument)

            updated_spaspm_object <- spaspm_object
            return(updated_spaspm_object)
          }
)
