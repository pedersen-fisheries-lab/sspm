#' Smooth a variable in a sspm dataset
#'
#' TODO
#'
#' @export
setGeneric(name = "spm_smooth",
           def = function(sspm_object,
                          ...){
             standardGeneric("spm_smooth")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname
setMethod(f = "spm_smooth",
          signature(sspm_object = ""),
          function(sspm_object, ...){
          }
)
