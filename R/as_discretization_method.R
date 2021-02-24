#' Cast into a `discretization_method` object
#'
#' Cast a character value into [`discretization_method`][discretization_method-class]
#' object, using the list of possible methods in [`spm_methods`][spm_methods].
#'
#' @param method **\[character\]** The name of the method.
#'
#' @return
#' An objectof class [`discretization_method`][discretization_method-class].
#'
#' @seealso [spm_methods].
#'
#' @export
setGeneric(name = "as_discretization_method",
           def = function(method){
             standardGeneric("as_discretization_method")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn as_discretization_method TODO
setMethod(f = "as_discretization_method",
          signature(method = "character"),
          function(method){


            # TODO review validity of this
            if(!checkmate::test_choice(method, spm_methods())){
              paste0("Method must be one of: ", paste0(spm_methods(),
                                                       collapse =  ", " ))
            }

            method_object <- new("discretization_method",
                                 name = method,
                                 method = dispatch_method(method))

            return(method_object)
          }
)
