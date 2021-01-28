#' Fit spatial gams
#'
#' @export
setGeneric(name = "spm_gam",
           def = function(spaspm_object,
                          ...){
             standardGeneric("spm_gam")
           }
)

#' @describeIn spm_gam TODO
#' @export
setMethod(f = "spm_gam",
          signature(spaspm_object = "spaspm_discrete"),
          function(spaspm_object, ...){

            # Step 1: do spatial join

            return(TRUE)

          }
)

#' @describeIn spm_gam TODO
#' @export
setMethod(f = "spm_gam",
          signature(spaspm_object = "spaspm"),
          function(spaspm_object, ...){

            message_not_discrete(spaspm_object)

          }
)

#' @describeIn spm_gam TODO
#' @export
setMethod(f = "spm_gam",
          signature(spaspm_object = "spaspm_gam_fit"),
          function(spaspm_object, force = FALSE, ...){

            checkmate::assert_logical(force)

            if (!force){
              message(paste0("Model '", spm_name(spaspm_object),
                             "' is already fitted with gams"))
              message("Use 'force = TRUE' to fit again")
            } else{

              message(paste0("Re-fitting gams for model '",
                             spm_name(spaspm_object), "'"))

              new_object <- new("spaspm_discrete",
                                name = spm_name(spaspm_object),
                                data = spm_data(spaspm_object)$data,
                                data_spatial = spm_data(spaspm_object)$data_spatial,
                                boundaries = spm_boundaries(spaspm_object),
                                method = spm_discret_method(spaspm_object),
                                patches = spm_patches(spaspm_object),
                                points = spm_points(spaspm_object))
              spm_gam(new_object,
                      ...)
            }

          }
)
