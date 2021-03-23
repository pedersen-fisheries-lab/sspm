#' Create a `sspm` model object
#'
#' The starting point of the `sspm` workflow: creating a base `sspm` object.
#'
#' @param model_name **\[character\]** The name to be given to the model
#' @param boundaries **\[sf\]** The spatial boundaries (polygons) for the model.
#'
#' @return
#' An object of class  [sspm][sspm-class].
#'
#' @rdname sspm-constructor
#' @export
sspm <- function(model_name = "My sspm model",
                 boundaries){

  checkmate::assert_character(model_name)
  checkmate::assert_class(boundaries, "sf")

  the_object <- new("sspm",
                    name = model_name,
                    #data = the_spapspm_data,
                    boundaries = boundaries)

  return(the_object)
}


