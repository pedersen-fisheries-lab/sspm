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
            base_formula <- (paste(response, "~", paste(other_terms, collapse = " + "),
                                   collapse = " "))

            # Capture calls and modify them
            smooth_calls <- lapply(smooth_terms_labels, str2lang)

            smooth_calls_modified <-
              lapply(smooth_calls, modify_call,
                     args = list(spaspm_object = substitute(spaspm_object),
                                 dataset = substitute(dataset)))

            # Evaluate the calls to get the args to make a smooth
            args_and_vars <-
              lapply(smooth_calls_modified, eval,
                     envir = list(. = spaspm_object))

            # Turn args into a call, and convert that call to a string
            string_smooths <-
              lapply(X = args_and_vars,
                     FUN = function(x){deparse(call2("s", !!!x$args),
                                               width.cutoff = 500)})

            # Paste them into formula
            final_formula <- paste0(base_formula, " + ",
                                    paste(string_smooths, collapse = " + "))

            # Cast as formula
            form_env <- list2env(sapply(args_and_vars, `[[`, "vars"))
            final_formula_casted <- as.formula(final_formula,
                                               env = form_env)

            spm_mapped_formulas(spaspm_object) <-
              append(spm_mapped_formulas(spaspm_object),
                     list(final_formula_casted))

            return(spaspm_object)
          }
)

# -------------------------------------------------------------------------

# assemble_smooth <- function(spaspm_object, dataset, dimension, ...){
#   # Evaluate the specific smooth
#   # browser()
#   the_smooth <- quote(s(x=dimension))
#   return(the_smooth)
# }

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
