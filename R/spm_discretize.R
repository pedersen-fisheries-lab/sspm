#' Discretize a `sspm` model object
#'
#' Discretize a [sspm][sspm-class] model object with a function from a
#' [method][method-class] object class.
#'
#' @param boundary_object **\[sspm\]** An object of class
#'    [sspm_boundary][sspm-class].
#' @param with **\[sspm_data OR sf\]** Either an object of class sspm_data or
#'    a set of custom points.
#' @param method **\[character OR method\]**
#'    Either a `character` from the list of available methods
#'    (see [spm_methods][spm_methods] for the list) **OR** an object of class
#'    [discretization_method][discretization_method-class].
#' @param ... **\[named list\]** Further arguments to be passed onto the function
#'    used in `method`.
#'
#' @return
#' An object of class [sspm_discrete_boundary][sspm-class] (the updated
#' and discretized `sspm` object given as input).
#'
#' @export
setGeneric(name = "spm_discretize",
           def = function(boundary_object,
                          with,
                          method = "tesselate_voronoi",
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
          signature(boundary_object = "ANY",
                    with = "ANY",
                    method = "missingOrNULL"),
          function(boundary_object, with, method, ...){
            stop("method argument missing.")
          }
)

# If missing arg, throw error
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "ANY",
                    with = "missingOrNULL",
                    method = "ANY"),
          function(boundary_object, with, method, ...){
            stop("with argument missing.")
          }
)

# If method is character, check against list, create `discretization_method`
# and call next signature.
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "ANY",
                    with = "ANY",
                    method = "character"),
          function(boundary_object, with, method, ...){

            method <- as_discretization_method(method)

            discrete <- spm_discretize(boundary_object, with, method, ...)
          }
)

# If with is sspm_data, subsitute for the data
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "sspm_boundary",
                    with = "sspm_data",
                    method = "discretization_method"),
          function(boundary_object, with, method, ...){

            with <- spm_data(with)

            discrete <- spm_discretize(boundary_object, with, method, ...)

          }
)

# sf case
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "sspm_boundary",
                    with = "sf",
                    method = "discretization_method"),
          function(boundary_object, with, method, ...){

            # Info message
            cli::cli_alert_info(paste0(" Discretizing using method ",
                                       cli::col_yellow(spm_name(method))))

            # Send to discretization routine
            boundaries <- boundary_object@boundaries
            boundary_column <- spm_boundary_colum(boundary_object)
            other_args <- list(...)

            discrete <-
              do.call(method_func(method),
                      args = append(list(boundaries = boundaries,
                                         boundary_column = boundary_column,
                                         with = with),
                                    other_args))

            # Check names of results
            checkmate::assert_names(x = names(discrete),
                                    subset.of = c("patches",
                                                  "points"))

            new_sspm_discrete_boundary <-
              new("sspm_discrete_boundary",
                  boundaries = boundary_object@boundaries,
                  boundary_column = boundary_column,
                  method = method,
                  patches = discrete[["patches"]],
                  points = discrete[["points"]])

            # JOIN all datasets
            # datasets <- spm_datasets(new_sspm_discrete)
            #
            # for (dataset_name in names(datasets)){
            #   sspm_data_tmp <- datasets[[dataset_name]]
            #   datasets[[dataset_name]] <- join_datasets(sspm_data_tmp, new_sspm_discrete)
            # }
            #
            # # Replace the objects
            # spm_datasets(new_sspm_discrete) <- datasets

            return(new_sspm_discrete_boundary)
          }
)

# RE-discretization not allowed for now
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "sspm_discrete_boundary",
                    with = "ANY",
                    method = "ANY"),
          function(boundary_object, with, method, ...){

            cli::cli_alert_danger(paste0(" Boundary is already discretized"))
          }
)
