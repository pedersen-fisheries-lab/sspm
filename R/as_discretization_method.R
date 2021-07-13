#' Cast into a `discretization_method` object
#'
#' Cast a character value into [`discretization_method`][discretization_method-class]
#' object, using the list of possible methods in [`spm_methods`][spm_methods].
#'
#' @param name **\[character\]** The name of the method.
#' @param method **\[character\]** The name of the method.
#'
#' @return
#' An objectof class [`discretization_method`][discretization_method-class].
#'
#' @seealso [spm_methods].
#'
#' @export
setGeneric(name = "as_discretization_method",
           def = function(name, method) {
             standardGeneric("as_discretization_method")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname as_discretization_method
setMethod(f = "as_discretization_method",
          signature(name = "character"),
          function(name) {

            method_f <- dispatch_method(name)

            if (!is.character(method_f)) {

              method_object <- new("discretization_method",
                                   name = name,
                                   method = method_f)

              return(method_object)
            }

          }
)

#' @export
#' @rdname as_discretization_method
setMethod(f = "as_discretization_method",
          signature(name = "missing", method = "function"),
          function(method) {

            method_object <- new("discretization_method",
                                 name = "Custom",
                                 method = method)

            return(method_object)

          }
)
