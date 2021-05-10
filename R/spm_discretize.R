#' Discretize a `sspm` model object
#'
#' Discretize a [sspm][sspm-class] model object with a function from a
#' [discretization_method][discretization_method-class] object class.
#'
#' @param sspm_object **\[sspm\]** An object of class
#'    [sspm][sspm-class].
#' @param with_dataset **\[character\]** The name of the dataset to use for the
#'    discretization.
#' @param discretization_method **\[character OR discretization_method\]**
#'    Either a `character` from the list of available methods
#'    (see [spm_methods][spm_methods] for the list) **OR** an object of class
#'    [discretization_method][discretization_method-class].
#' @param ... **\[named list\]** Further arguments to be passed onto the function
#'    used in the `discretization_method`.
#'
#' @return
#' An object of class [sspm_discrete][sspm-class] (the updated
#' and discretized `sspm` object given as input).
#'
#' @export
setGeneric(name = "spm_discretize",
           def = function(sspm_object,
                          with_dataset,
                          discretization_method,
                          ...){
             standardGeneric("spm_discretize")
           }
)

# Methods -----------------------------------------------------------------
# TODO finish the rdname description

# If invalid method, throw error
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "ANY",
                    with_dataset = "ANY",
                    discretization_method = "missingOrNULL"),
          function(sspm_object, with_dataset, discretization_method, ...){
            stop("Invalid discretization method.")
          }
)

# If missing arg, throw error
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "ANY",
                    with_dataset = "missing",
                    discretization_method = "ANY"),
          function(sspm_object, with_dataset, discretization_method, ...){
            stop("with_dataset argument missing.")
          }
)

# All signatures point to this one
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "sspm",
                    with_dataset = "character",
                    discretization_method = "discretization_method"),
          function(sspm_object, with_dataset, discretization_method, ...){

            # Get datasets
            datasets <- spm_datasets(sspm_object)

            # Check types
            checkmate::assert_class(discretization_method,"discretization_method")
            checkmate::assert_choice(with_dataset, c(names(datasets), "none"))

            # Get the dataset to use for discretization
            sspm_data <- datasets[[with_dataset]]

            if(with_dataset == "none"){

              # Message about mull with_dataset
              cli::cli_alert_info(paste0(" Argument with_dataset is set to none, assuming",
                                         " sample points have been passed on."))

            } else {

              # Get the dataset to use for discretization
              sspm_data <- datasets[[with_dataset]]

              # Info message
              cli::cli_alert_info(paste0(" Discretizing using method ",
                                         cli::col_yellow(spm_name(discretization_method)),
                                         " with dataset ",
                                         cli::col_green(with_dataset)))

            }

            # Send to discretization routine
            other_args <- list(...)

            # If boundaries are not overwritten, use th boundaries of sspm_object
            if(length(other_args) > 0){
              if (!is.element(other_args, "boundaries")){
                other_args$boundaries <- spm_boundaries(sspm_object)
              }
            } else if (length(other_args) == 0){
              other_args$boundaries <- spm_boundaries(sspm_object)
            }

            discrete <- do.call(method_func(discretization_method),
                                args = append(list(sspm_data = sspm_data),
                                              other_args))

            # Check names of results
            checkmate::assert_names(x = names(discrete),
                                    subset.of = c("patches",
                                                  "points"))

            # Replace datasets
            datasets[[with_dataset]] <- sspm_data

            new_sspm_discrete <- new("sspm_discrete",
                                     name = spm_name(sspm_object),
                                     datasets = datasets,
                                     boundaries = spm_boundaries(sspm_object),
                                     method = discretization_method,
                                     patches = discrete[["patches"]],
                                     points = discrete[["points"]])

            # JOIN all datasets
            datasets <- spm_datasets(new_sspm_discrete)

            for (dataset_name in names(datasets)){
              sspm_data_tmp <- datasets[[dataset_name]]
              datasets[[dataset_name]] <- join_datasets(sspm_data_tmp, new_sspm_discrete)
            }

            # Replace the objects
            spm_datasets(new_sspm_discrete) <- datasets

            return(new_sspm_discrete)
          }
)

# If `sspm` + character, check against list, create `discretization_method`
# and call next signature.
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "sspm",
                    with_dataset = "character",
                    discretization_method = "character"),
          function(sspm_object, with_dataset, discretization_method, ...){

            the_method <- as_discretization_method(discretization_method)

            discrete <- spm_discretize(sspm_object, with_dataset, the_method, ...)
          }
)

# RE-discretization not allowed for now
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "sspm_discrete",
                    with_dataset = "ANY",
                    discretization_method = "ANY"),
          function(sspm_object, with_dataset, discretization_method, ...){

            cli::cli_alert_danger(paste0(" Model '", spm_name(sspm_object),
                                         "' is already discretized"))
          }
)
