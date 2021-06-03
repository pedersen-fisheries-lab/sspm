
# Methods for extraction and selection generics ---------------------------

setMethod("$",
          "sspm_boundary",
          function(x, name) {
            x@boundaries[[name]]
          }
)

setMethod("$",
          "sspm_discrete_boundary",
          function(x, name) {
            x@boundaries[[name]]
          }
)

setMethod("$",
          "sspm_data",
          function(x, name) {
            x@data[[name]]
          }
)
