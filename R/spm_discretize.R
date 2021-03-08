#' Discretize a `sspm` model object
#'
#' Discretize a [sspm][sspm-class] model object with a function from a
#' [discretization_method][discretization_method-class] object class.
#'
#' @param sspm_object **\[sspm\]** An object of class
#'    [sspm][sspm-class].
#' @param discretization_method **\[character OR discretization_method\]**
#'    Either a `character` from the list of available methods
#'    (see [spm_methods][spm_methods] for the list) **OR** an object of class
#'    [discretization_method][discretization_method-class].
#' @param ... **\[named list\]** Further arguments to be passed onto the function
#'    used in the `discretization_method`.
#' @param force **\[boolean\]** Only used when calling `spm_discretize` onto
#'     an object of class [sspm_discrete][sspm-class]: whether you wish
#'     to force the re-discretization of that model.
#'
#' @return
#' An object of class [sspm_discrete][sspm-class] (the updated
#' and discretized `sspm` object given as input).
#'
#' @export
setGeneric(name = "spm_discretize",
           def = function(sspm_object,
                          discretization_method = "tesselate_voronoi",
                          ...){
             standardGeneric("spm_discretize")
           }
)

# Methods -----------------------------------------------------------------
# TODO finish the describeIn description

# All signatures point to this one
#' @describeIn spm_discretize TODO
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "sspm",
                    discretization_method = "discretization_method"),
          function(sspm_object, discretization_method, ...){

            checkmate::assert_class(discretization_method,
                                    "discretization_method")

            cli::cli_alert_info(paste0(" Discretizing using method '",
                                       discretization_method@name,"'"))

            other_args <- list(...)

            discrete <- do.call(method_func(discretization_method),
                                args = append(list(sspm_object = sspm_object),
                                              other_args))

            # Check names of results
            checkmate::assert_names(x = names(discrete),
                                    subset.of = c("data_spatial", "patches",
                                                  "points"))

            # Join data_spatial and patches
            discrete$data_spatial <-
              suppressMessages(sf::st_join(discrete$data_spatial,
                                           discrete$patches)) %>%
              dplyr::filter(!duplicated(.data[[spm_unique_ID(sspm_object)]]))

            sspm_object@data@data <- discrete$data_spatial

            new_sspm_discrete <- new("sspm_discrete",
                                     name = spm_name(sspm_object),
                                     data = spm_base_dataset(sspm_object),
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
                    discretization_method = "character"),
          function(sspm_object, discretization_method, ...){

            the_method <- as_discretization_method(discretization_method)

            discrete <- spm_discretize(sspm_object, the_method, ...)
          }
)

# If invalid method, throw error
#' @describeIn spm_discretize TODO
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "sspm",
                    discretization_method = "NULL"),
          function(sspm_object, discretization_method, ...){
            stop("Invalid discretization method.")
          }
)

# If `sspm_discrete` confirm that we want to re-discretize and then jump to
# the next appropriate signature

# SAME CODE than `sspm` + character
#' @describeIn spm_discretize TODO
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "sspm_discrete",
                    discretization_method = "character"),
          function(sspm_object, discretization_method, ...){

            the_method <- as_discretization_method(discretization_method)

            discrete <- spm_discretize(sspm_object, the_method, ...)
          }
)

#' @describeIn spm_discretize TODO
#' @export
setMethod(f = "spm_discretize",
          signature(sspm_object = "sspm_discrete",
                    discretization_method = "discretization_method"),
          function(sspm_object, discretization_method, force = FALSE, ...){

            checkmate::assert_logical(force)

            if (!force){

              cli::cli_alert_danger(paste0(" Model '", spm_name(sspm_object),
                                           "' is already discretized"))
              cli::cli_alert_info(" Use 'force = TRUE' to discretize again. but mapped datasets/smooths will be lost!")

            } else{

              cli::cli_alert_info(paste0(" Re-discretizing model '",
                                         spm_name(sspm_object), "'"))

              # Strip old geometry
              the_data <- spm_data(spm_base_dataset(sspm_object))
              st_geometry(the_data) <- NULL

              # Remove the columns that are in common
              names_to_remove <- names(spm_boundaries(sspm_object))
              names_to_remove <- names_to_remove[names_to_remove %in%
                                                   names(the_data)]

              the_data <- the_data %>%
                dplyr::select(-c(dplyr::all_of(names_to_remove), "patch_id", "area_km2"))

              new_object <- sspm(model_name = spm_name(sspm_object),
                                 name = spm_name(spm_base_dataset(sspm_object)),
                                 data = the_data,
                                 time_col = spm_time_col(sspm_object),
                                 coords = spm_coords_col(sspm_object),
                                 uniqueID = spm_unique_ID(sspm_object),
                                 boundaries = spm_boundaries(sspm_object))

              spm_discretize(new_object,
                             discretization_method = discretization_method,
                             ...)
            }
          }
)
