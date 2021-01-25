
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
               checkmate::assert_choice(discretization_method,
                                        all_methods_choices())
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
          function(spaspm_object, discretization_method){
            print("spm_discretize spaspm signature")

            # TODO MAKE the discretization method from character, then
            # call the the method again, this will dispatch to next one

          }
)

#' @export
setMethod(f = "spm_discretize",
          signature(spaspm_object = "spaspm",
                    discretization_method = "discretization_method"),
          function(spaspm_object, discretization_method){
            print("spm_discretize spaspm signature")

            # TODO call the actual discretization code

          }
)

#' @export
setMethod(f = "spm_discretize",
          signature(spaspm_object = "spaspm_discrete"),
          function(spaspm_object, discretization_method, force = FALSE){

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
                             discretization_method = discretization_method)
            }
          }
)
