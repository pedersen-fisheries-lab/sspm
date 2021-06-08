#' Create a `sspm` model object
#'
#' Create a sspm_model object.
#'
#' @param name **\[character\]** The name to be given to the model
#' @param boundaries **\[sf\]** The spatial boundaries (polygons) for the model.
#'
#' @return
#' An object of class  [sspm][sspm-class].
#'
#' @rdname sspm-constructor
#' @export
setGeneric(name = "sspm",
           def = function(boundaries,
                          biomass,
                          predictors,
                          catch,
                          biomass_var,
                          catch_var){
             standardGeneric("sspm")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(boundaries = "sspm_discrete_boundary",
                    biomass = "sspm_data",
                    predictors = "ANY",
                    catch = "missing"),
          function(boundaries,
                   biomass,
                   predictors = NULL,
                   catch = NULL){
            print("1")
          }
)

#' @export
#' @rdname sspm-constructor
setMethod(f = "sspm",
          signature(boundaries = "sspm_discrete_boundary",
                    biomass = "sspm_data",
                    predictors = "ANY",
                    catch = "sspm_data",
                    biomass_var = "character",
                    catch_var = "character"),
          function(boundaries,
                   biomass,
                   predictors,
                   catch,
                   biomass_var,
                   catch_var){
            print("2")
          }
)
