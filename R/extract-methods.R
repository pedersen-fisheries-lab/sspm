
# Methods for extraction and selection generics ---------------------------

setMethod("$",
          "sspm_boundary",
          function(x, name) {
            x@boundaries %>%
              dplyr::select(c(name, spm_boundary_colum(x), "geometry"))
          }
)

setMethod("$",
          "sspm_discrete_boundary",
          function(x, name) {
            x@boundaries %>%
              dplyr::select(c(name, spm_boundary_colum(x), "geometry"))
          }
)

setMethod("$",
          "sspm_data",
          function(x, name) {
            if (is.null(x@smoothed_data)){
              x@data %>%
                dplyr::select(c(name, spm_time_column(x), "geometry"))
            }else{
              x@smoothed_data %>%
                dplyr::select(c(name, spm_time_column(x), "geometry"))
            }
          }
)
