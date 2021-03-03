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

            method_f <- dispatch_method(method)

            if (!is.character(method_f)){

              method_object <- new("discretization_method",
                                   name = method,
                                   method = method_f)

              return(method_object)
            }

          }
)
