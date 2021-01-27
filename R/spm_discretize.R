#' Discretize a `spaspm` model object
#'
#' Discretize a [spaspm][spaspm-class] model object with a function from a
#' [discretization_method][discretization_method-class] object class.
#'
#' @param spaspm_object **\[spaspm\]**
#' @param discretization_method **\[character OR discretization_method\]**
#'    Either a `character` from the list of available methods
#'    (see [spm_methods][spm_methods] for the list) **OR** an object of class
#'    [discretization_method][discretization_method-class].
#' @param ... **\[named list\]** Further arguments to be passed onto the function used in the
#'    `discretization_method`.
#' @param force **\[boolean\]** Only used when calling `spm_discretize` onto
#'     an object of class [spaspm_discrete][spaspm-class]: whether you wish
#'     to force the re-discretization of that model.
#'
#' @return
#' An object of class [spaspm_discrete][spaspm-class] (the updated
#' and discretized `spaspm` object given as input).
#'
#' @export
setGeneric(name = "spm_discretize",
           def = function(spaspm_object,
                          discretization_method = "tesselate_voronoi",
                          ...){
             standardGeneric("spm_discretize")
           }
)

# Methods -----------------------------------------------------------------
# TODO finish the describeIn description

# If `spaspm` + character, check against list, create `discretization_method`
# and call next signature.
#' @describeIn spm_discretize TODO
#' @export
setMethod(f = "spm_discretize",
          signature(spaspm_object = "spaspm",
                    discretization_method = "character"),
          function(spaspm_object, discretization_method, ...){

            if(!checkmate::test_choice(discretization_method, spm_methods())){
              paste0("Method must be one of: ", paste0(spm_methods(),
                                                       collapse =  ", " ))
            }

            the_method <- new("discretization_method",
                              name = discretization_method,
                              method = dispatch_method(discretization_method))

            discrete <- spm_discretize(spaspm_object, the_method, ...)
          }
)

# All signatures point to this one
#' @describeIn spm_discretize TODO
#' @export
setMethod(f = "spm_discretize",
          signature(spaspm_object = "spaspm",
                    discretization_method = "discretization_method"),
          function(spaspm_object, discretization_method, ...){

            checkmate::assert_class(discretization_method,
                                    "discretization_method")

            message(paste0("Discretizing using method '",
                           discretization_method@name,"'"))

            other_args <- list(...)

            # TODO get slot with actual method here
            discrete <- do.call(discretization_method@method,
                                args = append(list(spaspm_object = spaspm_object),
                                              other_args))

            new_spaspm_discrete <- new("spaspm_discrete",
                                       data = spm_data(spaspm_object),
                                       boundaries = spm_boundaries(spaspm_object),
                                       data_spatial = discrete[["data_spatial"]],
                                       method = discretization_method,
                                       patches = discrete[["patches"]],
                                       points = discrete[["points"]])

            return(new_spaspm_discrete)
          }
)

# If `spaspm_discrete` confirm that we want to re-discretize and then jump to
# the next appropriate signature
#' @describeIn spm_discretize TODO
#' @export
setMethod(f = "spm_discretize",
          signature(spaspm_object = "spaspm_discrete"),
          function(spaspm_object, discretization_method, force = FALSE, ...){

            checkmate::assert_logical(force)

            if (!force){
              message(paste0("Model '", spm_name(spaspm_object),
                             "' is already discretized"))
              message("Use 'force = TRUE' to discretize again")
            } else{

              message(paste0("Re-discretizing model '",
                             spm_name(spaspm_object), "'"))

              new_object <- new("spaspm",
                                data = spm_data(spaspm_object)$data,
                                boundaries = spm_boundaries(spaspm_object))
              spm_discretize(new_object,
                             discretization_method = discretization_method,
                             ...)
            }
          }
)
