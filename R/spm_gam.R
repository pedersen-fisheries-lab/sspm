#' Fit spatial gams
#'
#' TBD.
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'    [sspm_discrete][sspm_discrete-class].
#' @inheritParams spm_discretize
#' @param ... TBD
#'
#' @return
#' TBD.
#'
#' @export
setGeneric(name = "spm_gam",
           def = function(sspm_object,
                          ...){
             standardGeneric("spm_gam")
           }
)

#' @describeIn spm_gam TODO
#' @export
setMethod(f = "spm_gam",
          signature(sspm_object = "sspm_discrete"),
          function(sspm_object, ...){

            # Step 1: do spatial join

            return(TRUE)

          }
)

#' @describeIn spm_gam TODO
#' @export
setMethod(f = "spm_gam",
          signature(sspm_object = "sspm"),
          function(sspm_object, ...){

            message_not_discrete(sspm_object)

          }
)

#' @describeIn spm_gam TODO
#' @export
setMethod(f = "spm_gam",
          signature(sspm_object = "sspm_gam_fit"),
          function(sspm_object, force = FALSE, ...){

            checkmate::assert_logical(force)

            if (!force){
              message(paste0("Model '", spm_name(sspm_object),
                             "' is already fitted with gams"))
              message("Use 'force = TRUE' to fit again")
            } else{

              message(paste0("Re-fitting gams for model '",
                             spm_name(sspm_object), "'"))

              new_object <- new("sspm_discrete",
                                name = spm_name(sspm_object),
                                data = spm_data(sspm_object)$data,
                                data_spatial = spm_data(sspm_object)$data_spatial,
                                boundaries = spm_boundaries(sspm_object),
                                method = spm_discret_method(sspm_object),
                                patches = spm_patches(sspm_object),
                                points = spm_points(sspm_object))
              spm_gam(new_object,
                      ...)
            }

          }
)
