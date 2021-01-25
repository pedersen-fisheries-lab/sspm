# Generic -----------------------------------------------------------------

# Takes in spaspm and spits out the spaspm_discrete
#' @export
setGeneric(name = "spm_discretize",
           def = function(spaspm_object,
                          discretization_method = "tesselate_voronoi",
                          ...){

             # Check first if character and if parts of possible choices
             # if not, check if object of class discretization_method
             if(checkmate::test_character(discretization_method)){
               if(!checkmate::test_choice(discretization_method, spm_methods())){
                 paste0("Method must be one of: ", paste0(spm_methods(),
                                                          collapse =  ", " ))
               }

             } else {

               checkmate::assert_class(discretization_method,
                                       "discretization_method")
             }

             standardGeneric("spm_discretize")
           }
)

# Methods -----------------------------------------------------------------

#' @export
setMethod(f = "spm_discretize",
          signature(spaspm_object = "spaspm",
                    discretization_method = "character"),
          function(spaspm_object, discretization_method, ...){

            the_method <- new("discretization_method",
                              name = discretization_method,
                              method = dispatch_method(discretization_method),
                              boundaries = spm_boundaries(spaspm_object))

            spm_discretize(spaspm_object, the_method, ...)

          }
)

#' @export
setMethod(f = "spm_discretize",
          signature(spaspm_object = "spaspm",
                    discretization_method = "discretization_method"),
          function(spaspm_object, discretization_method, ...){

            message(paste0("Discretizing using method '",
                           discretization_method@name,"'"))

            other_args <- list(...)

            # TODO get slot with actual method here
            results <- do.call(discretization_method@method,
                               args = append(list(spaspm_object = spaspm_object),
                                             other_args))

            return(results)
          }
)

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
                                data = spm_data(spaspm_object),
                                boundaries = spm_boundaries(spaspm_object))
              spm_discretize(new_object,
                             discretization_method = discretization_method,
                             ...)
            }
          }
)
