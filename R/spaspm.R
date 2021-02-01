#' @import sf
#' @importFrom rlang .data
#' @importFrom methods new show

# Model Object Constructor
#' @export
spaspm <- function(name="SPSPM Model",
                   data,
                   ID,
                   boundaries,
                   # discretize = TRUE,
                   # discretization_method = "tesselate_voronoi",
                   # fit_gam = TRUE,
                   ...){

  if(!checkmate::test_subset(ID, names(data))){
    stop("`ID` must be a column of `data`")
  }

  the_object <- new("spaspm",
                    name = name,
                    ID = ID,
                    data = data,
                    boundaries = boundaries)

  return(the_object)
}
