# Methods -----------------------------------------------------------------

custom_h1 <- function(text){
  cli::cat_line(
    cli::symbol$figure_dash, cli::symbol$figure_dash,
    " ", cli::style_bold(text), " ",
    cli::symbol$figure_dash, cli::symbol$figure_dash)
}

setMethod("show",
          "sspm",
          function(object) {
            cli::cat_line()
            custom_h1(paste0("SSPM object '", object@name, "'"))
            cat_boundaries(object)
            cat_datasets(object)
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_discrete",
          function(object) {
            cli::cat_line()
            custom_h1(paste0("SSPM object '", object@name, "' ",
                             cli::col_green("(DISCRETIZED)")))
            cat_boundaries(object)
            cat_discretization_info(object)
            cat_datasets(object)
            if (length(object@mapped_formulas) >= 1){
              cat_mapped_formulas(object)
            }
            cli::cat_line()
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
            cli::cat_bullet(" Time column        : ", cli::col_cyan(object@time_column))
            if(!is.null(object@coords)){
              cli::cat_bullet(" Coordinates cols   : ",
                              paste(cli::col_green(object@coords), collapse = ", "))
            }
          }
)

setMethod("show",
          "sspm_formula",
          function(object) {
            cli::cli_h3(cli::col_cyan("SSPM Formula for dataset ", cli::col_magenta(object@dataset) ))
            cli::cat_bullet(" Raw formula        : ", format_formula(object@raw_formula))
            cli::cat_bullet(" Translated formula : ", format_formula(object@translated_formula))
            cli::cat_bullet(" Variables          : ", paste0(names(object@vars), collapse = ", "))
          }
)

# Helpers -----------------------------------------------------------------

cat_boundaries <- function(object){

  dim_1 <- dim(object@boundaries)[1]
  dim_2 <- dim(object@boundaries)[2]

  cli::cat_bullet(cli::col_cyan(" Boundaries       : "),
                  cli::pluralize("[ {dim_1} feature{?s} ; {dim_2} variable{?s} ]"),
                  bullet = "arrow_right")

}

cat_datasets <- function(object){

  datasets <- spm_datasets(object)
  len_dat <- length(datasets)


  cli::cat_bullet(paste0(cli::col_cyan(" Datasets         : "),
                         cli::pluralize("{no(len_dat)} dataset{?s}")),
                  bullet = "arrow_right")

  if(len_dat > 0){

    for(i in seq_len(length.out = length(datasets))){

      the_dataset <- datasets[[i]]
      the_dataset_formulas <- the_dataset@mapped_formulas

      dim_1 <- dim(spm_data(the_dataset))[1]
      dim_2 <- dim(spm_data(the_dataset))[2]

      the_line <-
        paste(cli::symbol$star, cli::col_green(spm_name(the_dataset)),
              cli::symbol$em_dash,
              cli::pluralize("[ {dim_1} observation{?s} ; {dim_2} variable{?s} ]"))

      if(the_dataset@is_smoothed == TRUE){
        the_line <-
          paste(the_line, cli::col_green(cli::style_bold("(SMOOTHED)")))
      }

      cli::cat_line("   ", the_line)

      if(length(the_dataset_formulas)>0){
        for (form in the_dataset_formulas){
          formatted <- format_formula(form@raw_formula)
          cli::cat_line("      ", cli::symbol$en_dash, " ",
                        cli::col_yellow(paste0(strtrim(formatted, 70), "...")))
        }
      }
    }
  }
}

cat_discretization_info <- function(object){

  cli::cat_bullet(cli::col_cyan(" Discretized with : "),
                  bullet = "arrow_right")

  points_dim_1 <- dim(object@points)[1]
  points_dim_2 <- dim(object@points)[2]
  cli::cat_line("   ", paste(cli::symbol$star, cli::col_green("Points"),
                             cli::symbol$em_dash,
                             cli::pluralize("[ {points_dim_1} feature{?s} ; {points_dim_2} variable{?s} ]")))

  patches_dim_1 <- dim(object@patches)[1]
  patches_dim_2 <- dim(object@patches)[2]
  cli::cat_line("   ", paste(cli::symbol$star, cli::col_green("Patches"),
                             cli::symbol$em_dash,
                             cli::pluralize("[ {patches_dim_1} feature{?s} ; {patches_dim_2} variable{?s} ]")))

}

########

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

########

format_formula <- function(form){
  gsub(format(
    paste0(trimws(format(form)), collapse = " ")
  ), pattern = "\\\"", replacement="'")
}

