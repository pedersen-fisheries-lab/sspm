# Exported ----------------------------------------------------------------

#' Get the list of available discretization methods
#'
#' Currently, only one discretization method is supported:
#'     * `"tesselate_voronoi"` Voronoi tessellation using the function
#'       [tesselate_voronoi][tesselate_voronoi].
#'
#' You can create your own method (tutorial TBD).
#'
#' @return
#' A `character vector` of all available discretization methods.
#'
#' @export
spm_methods <- function() {
  choices <- c('tesselate_voronoi', 'triangulate_delaunay')
  return(choices)
}

#' Get the list of available smoothing methods
#'
#' Currently, only one smoothing method is supported:
#'     * `"ICAR"`: Intrinsic Conditional Auto-Regressive models.
#'
#' @return
#' A `character vector` of all available smoothing methods.
#'
#' @export
spm_smooth_methods <- function() {
  choices <- c('ICAR', 'LINPRED')
  return(choices)
}

# Not exported ------------------------------------------------------------

spm_aggregation_choices <- function() {
  choices <- c('space', 'time', 'spacetime')
  return(choices)
}

# Join datasets to patches
join_datasets <- function(sspm_dataset, sspm_boundary) {

  checkmate::assert_class(sspm_dataset, "sspm_dataset")
  checkmate::assert_class(sspm_boundary, "sspm_discrete_boundary")

  the_data <- spm_data(sspm_dataset)
  the_patches <- sspm_boundary@patches

  # TODO REVIEW THE COHERENCE OF ST_TRANSFORM
  joined <- suppressMessages(sf::st_transform(the_data, crs = sf::st_crs(the_patches)))
  # TODO joining patches to points but should be the opposite, keeping for rep for now
  joined <- suppressMessages(sf::st_join(the_patches, the_data,
                                         suffix	= c("", "_dup"))) %>%
    dplyr::filter(!duplicated(.data[[spm_unique_ID(sspm_dataset)]])) %>%
    dplyr::filter(!is.na(.data$patch_id))

  spm_data(sspm_dataset) <- joined
  spm_boundaries(sspm_dataset) <- sspm_boundary
  is_mapped(sspm_dataset) <- TRUE

  return(sspm_dataset)
}

# Suppress both messages and warnings
suppressAll <- function(x) {
  suppressWarnings(suppressMessages(x))
}
