# Methods -----------------------------------------------------------------

setMethod("show",
          "spaspm",
          function(object) {
            cli::cli_h2(cli::col_blue(cli::style_bold("SPASPM object '",
                                                      object@name, "'")))
            show(object@data)
            cat_boundaries(object)
            cat("\n")
          }
)

setMethod("show",
          "spaspm_discrete",
          function(object) {
            cli::cli_h2(cli::col_blue(cli::style_bold("SPASPM object '",
                                                      object@name, "' ",
                                                      cli::col_green("(DISCRETIZED)"))))
            show(object@data)
            cat_boundaries(object)
            cat_discretization_info(object)
            if (length(object@mapped_datasets) >= 1){
              cat_mapped_datasets(object)
            }
            if (length(object@mapped_smooths) >= 1){
              cat_mapped_smooths(object)
            }
            cat("\n")
          }
)

setMethod("show",
          "discretization_method",
          function(object) {
            cli::cli_h3(cli::col_cyan("Discretization method"))
            cli::cat_bullet(" Name               : '", object@name, "'")
            # TODO manage to print function name
            # cli::cat_bullet(" Function         :", object@method)
          }
)

setMethod("show",
          "spaspm_data",
          function(object) {
            cli::cli_h3(cli::col_cyan("SPASPM Dataset '", object@name, "' "))
            cli::cat_bullet(" Data matrix        : ", object@representation, " with ",
                            dim(object@data)[1], " feature(s) and ",
                            dim(object@data)[2], " variable(s)")
            cli::cat_bullet(" Data unique ID     : ", cli::col_cyan(object@uniqueID))
            cli::cat_bullet(" Time column        : ", cli::col_cyan(object@time_col))
            if(!is.null(object@coords)){
              cli::cat_bullet(" Coordinates cols   : ",
                              paste(cli::col_green(object@coords), collapse = ", "))
            }
          }
)

# Helpers -----------------------------------------------------------------

cat_boundaries <- function(object){
  cli::cli_h3(cli::col_cyan("Boundaries"))
  cli::cat_bullet(" Boundary data      : ", "Simple feature collection with ",
                  dim(object@boundaries)[1] ," feature(s) and ",
                  dim(object@boundaries)[2], " variable(s)")
}

cat_discretization_info <- function(object){
  cli::cli_h3(cli::col_cyan("Discretization info"))
  cli::cat_bullet(" Method name        : '", object@method@name, "'")
  cli::cat_bullet(" Patches            : ", "Simple feature collection with ",
                  dim(object@patches)[1] ," patches (and ",
                  dim(object@patches)[2], " field(s))")
  cli::cat_bullet(" Points             : ", "Simple feature collection with ",
                  dim(object@points)[1] ," points (and ",
                  dim(object@points)[2], " field(s))")
}

cat_mapped_datasets <- function(object){
  datasets <- object@mapped_datasets
  cli::cli_h3(cli::col_cyan("Mapped Datasets"))
  cli::cat_bullet(" ", cli::col_cyan(length(datasets)),
             " mapped dataset(s): ", paste(cli::col_magenta(sapply(datasets ,
                                                                   spm_name)),
                                           collapse = ", "))
}

cat_mapped_smooths <- function(object){
  smooths <- object@mapped_smooths
  cli::cli_h3(cli::col_cyan("Mapped smooths"))
  cli::cat_bullet(cli::col_red("TODO"))
}
