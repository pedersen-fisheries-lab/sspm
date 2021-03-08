# Methods -----------------------------------------------------------------

setMethod("show",
          "sspm",
          function(object) {
            cli::cli_h2(cli::col_blue(cli::style_bold("sspm object '",
                                                      object@name, "'")))
            show(object@data)
            cat_boundaries(object)
            cat("\n")
          }
)

setMethod("show",
          "sspm_discrete",
          function(object) {
            cli::cli_h2(cli::col_blue(cli::style_bold("sspm object '",
                                                      object@name, "' ",
                                                      cli::col_green("(DISCRETIZED)"))))
            show(object@data)
            cat_boundaries(object)
            cat_discretization_info(object)
            if (length(object@mapped_datasets) >= 1){
              cat_mapped_datasets(object)
            }
            if (length(object@mapped_formulas) >= 1){
              cat_mapped_formulas(object)
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
          "sspm_data",
          function(object) {
            cli::cli_h3(cli::col_cyan("Base dataset '", object@name, "' "))
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

setMethod("show",
          "sspm_formula",
          function(object) {
            cli::cli_h3(cli::col_cyan("sspm Formula for dataset ", cli::col_magenta(object@dataset) ))
            cli::cat_bullet(" Raw formula        : ", format_formula(object@raw_formula))
            cli::cat_bullet(" Translated formula : ", format_formula(object@translated_formula))
            cli::cat_bullet(" Variables          : ", paste0(names(object@vars), collapse = ", "))
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
                  " mapped dataset(s): ", paste(cli::col_cyan(sapply(datasets ,
                                                                     spm_name)),
                                                collapse = ", "))
}

cat_mapped_formulas <- function(object){
  formulas <- object@mapped_formulas
  cli::cli_h3(cli::col_cyan("Mapped formulas"))
  for(form_id in seq_len(length.out = length(formulas))){
    the_formula <- formulas[[form_id]]
    formatted <- format_formula(the_formula@raw_formula)
    cli::cat_line(cli::col_cyan(paste0(form_id, ") ")),
                  cli::col_yellow(paste0(strtrim(formatted, 70), "...")),
                  " for dataset ", cli::col_cyan(paste0("'", the_formula@dataset, "'")))
  }
}

format_formula <- function(form){
  gsub(format(
    paste0(trimws(format(form)), collapse = " ")
  ), pattern = "\\\"", replacement="'")
}
