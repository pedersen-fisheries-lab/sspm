# Methods -----------------------------------------------------------------

custom_h1 <- function(text){
  cli::cat_line(
    cli::symbol$figure_dash, cli::symbol$figure_dash,
    " ", cli::style_bold(text), " ",
    cli::symbol$figure_dash, cli::symbol$figure_dash)
}

custom_h3 <- function(text){
  cli::cat_line(
    cli::symbol$figure_dash,
    " ", cli::style_bold(text), " ")
}

setMethod("show",
          "sspm_boundary",
          function(object) {
            cli::cat_line()
            custom_h1(paste0("SSPM Boundary"))
            cat_boundaries(object, column = TRUE)
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_discrete_boundary",
          function(object) {
            cli::cat_line()
            custom_h1(paste0("SSPM Boundary ", cli::col_green("(Discrete)")))
            cat_boundaries(object, column = TRUE)
            cat_discretization_info(object)
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_data",
          function(object) {
            cli::cat_line()
            custom_h1(paste0("SSPM Dataset: ", cli::col_blue(object@name),
                             cli::col_magenta(" (", object@type, ")")))
            cat_data(object)
            cat_formulas(object)
            cat_boundaries(object, column = FALSE)
            cat_smoothed_data(object)
            cli::cat_line()
          }
)

setMethod("show",
          "discretization_method",
          function(object) {
            cli::cat_line()
            custom_h1("Discretization method")
            cli::cat_bullet(" Name : '", object@name, "'",
                            bullet = "arrow_right")
            # TODO Print function
            # cli::cat_bullet(" Function         :", substitute(object@method),
            #                 bullet = "em_dash")
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_formula",
          function(object) {
            cli::cat_line()
            custom_h1("SSPM Formula")
            cli::cat_bullet(" Raw formula        : ",
                            format_formula(object@raw_formula),
                            bullet = "arrow_right")
            cli::cat_bullet(" Translated formula : ",
                            format_formula(object@translated_formula),
                            bullet = "arrow_right")
            cli::cat_bullet(" Variables          : ",
                            paste0(names(object@vars), collapse = ", "),
                            bullet = "arrow_right")
            cli::cat_line()
          }
)

# Helpers -----------------------------------------------------------------

cat_boundaries <- function(object, column = TRUE){

  if(column){
    cli::cat_bullet(" Boundaries    : ",
                    pluralize_data_info(object@boundaries),
                    bullet = "arrow_right")

    cli::cat_bullet(" Boundary col. : ",
                    cli::col_blue(object@boundary_column),
                    bullet = "arrow_right")
  } else {

    cli::cat_bullet(" Boundaries        : ",
                    pluralize_data_info(object@boundaries@boundaries),
                    bullet = "arrow_right")

  }

}

cat_discretization_info <- function(object){

  cli::cat_bullet(" Discretized   : ",
                  bullet = "arrow_right")

  cli::cat_line("   ", paste(cli::symbol$star, cli::col_green("Points"),
                             cli::symbol$em_dash,
                             pluralize_data_info(object@points, dim_1_name = "feature")))
  cli::cat_line("   ", paste(cli::symbol$star, cli::col_green("Patches"),
                             cli::symbol$em_dash,
                             pluralize_data_info(object@patches, dim_1_name = "feature")))

}

cat_data <- function(object){

  if(object@is_mapped){
    header <- paste0(" Data ", cli::col_blue("(MAPPED)"),
                     "     : ")
  } else {
    header <- " Data              : "
  }

  cli::cat_bullet(header,
                  pluralize_data_info(object@data),
                  bullet = "arrow_right")
  cli::cat_bullet(" Data unique ID    : ",
                  cli::col_blue(object@uniqueID),
                  bullet = "arrow_right")
  cli::cat_bullet(" Time col.         : ",
                  cli::col_blue(object@time_column),
                  bullet = "arrow_right")
  if(!is.null(object@coords)){
    cli::cat_bullet(" Coordinates cols. : ",
                    paste(cli::col_green(object@coords),
                          collapse = ", "),
                    bullet = "arrow_right")
  }

}

cat_formulas <- function(object){

  the_dataset_formulas <- spm_formulas(object)

  if(length(the_dataset_formulas) != 0){

    cli::cat_bullet(" Formulas          : ",
                    bullet = "arrow_right")

    for (f_id in seq_len(length.out = length(the_dataset_formulas))){

      form <- the_dataset_formulas[[f_id]]
      formatted <- cat_formula(form@raw_formula)

      cli::cat_line("      ", cli::symbol$en_dash, " ",
                    cli::col_yellow(formatted))

    }

  }

}

cat_smoothed_data <- function(object){

  if(!is.null(object@smoothed_data)){

    cli::cat_bullet(" Smoothed Data     : ",
                    pluralize_data_info(object@smoothed_data),
                    bullet = "arrow_right")

    columns_with_smooth <-
      names(which(sapply(colnames(object@smoothed_data),
                         grepl, pattern = "_smooth", fixed=TRUE)))

    if (length(columns_with_smooth) > 0){
      the_line <-
        paste(cli::symbol$star, "smoothed vars:",
              paste(cli::col_green(sort(columns_with_smooth)),
                    collapse = paste0(" ", cli::symbol$em_dash, " ")))

      cli::cat_line("   ", the_line)
    }

  }

}

# -------------------------------------------------------------------------

pluralize_data_info <- function(object,
                                dim_1_name = "observation",
                                dim_2_name = "variable"){

  dim_1 <- dim(object)[1]
  dim_2 <- dim(object)[2]

  info <-
    cli::pluralize("[", cli::col_blue("{dim_1}")," ", dim_1_name, "{?s}, ",
                   cli::col_blue("{dim_2}"), " ", dim_2_name, "{?s}]")

  return(info)

}

format_formula <- function(form){
  gsub(format(
    paste0(trimws(format(form)), collapse = " ")
  ), pattern = "\\\"", replacement="'")
}

cat_formula <- function(formula, max_length=1000){
  formatted <- format_formula(formula)
  if(nchar(formatted)>max_length){
    formatted <- paste0(strtrim(formatted, max_length), "...")
  }
  return(formatted)
}
