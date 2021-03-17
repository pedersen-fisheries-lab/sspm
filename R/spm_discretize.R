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
# TODO finish the describeIn description

# If invalid method, throw error
#' @describeIn spm_discretize TODO
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
#' @describeIn spm_discretize TODO
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
#' @describeIn spm_discretize TODO
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
            checkmate::assert_choice(with_dataset, names(datasets))

            # Get the dataset to use for discretization
            sspm_data <- datasets[[with_dataset]]

            # Info message
            cli::cli_alert_info(paste0(" Discretizing using method ",
                                       cli::col_yellow(spm_name(discretization_method)),
                                       " with dataset ",
                                       cli::col_green(with_dataset)))

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
                                    subset.of = c("data_spatial",
                                                  "patches",
                                                  "points"))

            # Replace datasets
            spm_data(sspm_data) <- discrete$data_spatial
            datasets[[with_dataset]] <- sspm_data

            # JOIN otehr datasetssspm_object
            other_names <- names(datasets)[!(names(datasets) %in% "b")]

            if (length(other_names) >1 ){
              for (dataset_name in other_names){
                sspm_data_tmp <- datasets[[dataset_name]]
                datasets[[dataset_name]] <- join_datasets(sspm_data_tmp, sspm_object)
              }
            }

            # Replace the objects
            spm_datasets(sspm_object) <- datasets

            new_sspm_discrete <- new("sspm_discrete",
                                     name = spm_name(sspm_object),
                                     datasets = spm_datasets(sspm_object),
                                     boundaries = spm_boundaries(sspm_object),
                                     method = discretization_method,
                                     patches = discrete[["patches"]],
                                     points = discrete[["points"]])

            return(new_sspm_discrete)
          }
)

# If `sspm` + character, check against list, create `discretization_method`
# and call next signature.
#' @describeIn spm_discretize TODO
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

# If `sspm_discrete` confirm that we want to re-discretize and then jump to
# the next appropriate signature

# SAME CODE than `sspm` + character
#' @describeIn spm_discretize TODO
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "sspm_discrete",
                    with_dataset = "character",
                    discretization_method = "character"),
          function(sspm_object, with_dataset, discretization_method, ...){

            the_method <- as_discretization_method(discretization_method)

            discrete <- spm_discretize(sspm_object, with_dataset, the_method, ...)
          }
)

# RE-discretization not allowed for now
#' @describeIn spm_discretize TODO
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


## IMPORTANT Join helper
join_datasets <- function(sspm_data, sspm_object){

  checkmate::assert_class(sspm_data, "sspm_data")
  checkmate::assert_class(sspm_object, "sspm_discrete")

  the_data <- spm_data(sspm_data)
  the_patches <- spm_patches(sspm_object)

  # TODO REVIEW THE COHERENCE OF ST_TRANSFORM
  joined <- suppressMessages(sf::st_transform(the_data, crs = sf::st_crs(the_patches)))
  joined <- suppressMessages(sf::st_join(the_data, the_patches)) %>%
    dplyr::filter(!duplicated(.data[[spm_unique_ID(sspm_data)]]))

  spm_data(sspm_data) <- joined

  return(sspm_data)
}
