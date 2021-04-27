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
            custom_h1(paste0(" SSPM object '", object@name, "' ",
                             cli::style_italic("[DISCRETIZED] ")))
            cat_boundaries(object)
            cat_discretization_info(object)
            cat_datasets(object)
            cat_smoothed_data(object)
            cli::cat_line()
          }
)

setMethod("show",
          "discretization_method",
          function(object) {
            custom_h3(cli::col_cyan("Discretization method"))
            cli::cat_bullet(" Name : '", object@name, "'",
                            bullet = "em_dash")
            # cli::cat_bullet(" Function         :", substitute(object@method),
            #                 bullet = "em_dash")
          }
)

setMethod("show",
          "sspm_data",
          function(object) {
            custom_h3(cli::col_cyan("Dataset '", object@name, "' "))

            dim_1 <- dim(spm_data(object))[1]
            dim_2 <- dim(spm_data(object))[2]
            cli::cat_bullet(" Data             : ",
                            cli::pluralize("[", cli::col_blue("{dim_1}")," observation{?s}, ",
                                           cli::col_blue("{dim_2}"), " variable{?s}]"),
                            bullet = "em_dash")

            cli::cat_bullet(" Data unique ID   : ",
                            cli::col_cyan(object@uniqueID),
                            bullet = "em_dash")
            cli::cat_bullet(" Time column      : ",
                            cli::col_cyan(object@time_column),
                            bullet = "em_dash")
            if(!is.null(object@coords)){
              cli::cat_bullet(" Coordinates cols : ",
                              paste(cli::col_green(object@coords),
                                    collapse = ", "),
                              bullet = "em_dash")
            }
          }
)

setMethod("show",
          "sspm_formula",
          function(object) {
            custom_h3(cli::col_cyan("SSPM Formula"))
            cli::cat_bullet(" Raw formula        : ",
                            format_formula(object@raw_formula),
                            bullet = "em_dash")
            cli::cat_bullet(" Translated formula : ",
                            format_formula(object@translated_formula),
                            bullet = "em_dash")
            cli::cat_bullet(" Variables          : ",
                            paste0(names(object@vars), collapse = ", "),
                            bullet = "em_dash")
          }
)

# Helpers -----------------------------------------------------------------

cat_boundaries <- function(object){

  cli::cat_bullet(cli::col_cyan(" Boundaries    : "),
                  pluralize_data_info(object@boundaries),
                  bullet = "arrow_right")

}

cat_datasets <- function(object){

  datasets <- spm_datasets(object)
  len_dat <- length(datasets)

  cli::cat_bullet(cli::col_cyan(" Datasets      : "),
                  cli::pluralize(cli::col_cyan("{no(len_dat)}")," dataset{?s}"),
                  bullet = "arrow_right")

  if(len_dat > 0){

    for(i in seq_len(length.out = length(datasets))){

      the_dataset <- datasets[[i]]
      the_dataset_formulas <- the_dataset@formulas

      the_line <-
        paste(cli::symbol$star, cli::col_green(spm_name(the_dataset)),
              cli::col_magenta(paste0("(", spm_type(the_dataset), ")")),
              cli::symbol$em_dash,
              pluralize_data_info(spm_data(the_dataset)))

      cli::cat_line("   ", the_line)

      if(length(the_dataset_formulas)>0){

        for (f_id in seq_len(length.out = length(the_dataset_formulas))){

          form <- the_dataset_formulas[[f_id]]
          formatted <- cat_formula(form@raw_formula)

          if(the_dataset@is_smoothed == TRUE){

            # the_smoothed_data <- the_dataset@smoothed_data[[f_id]]
            # base_line <- paste0("      ", cli::symbol$en_dash, " ")

            the_tag <- "(SMOOTHED)"

            # smoothed <- paste0(" [", cli::col_blue(nrow(the_smoothed_data)), " observations]")
            # the_line <- paste0(base_line, cli::col_green(cli::style_bold(the_tag)), smoothed)
            # the_line <- paste0(base_line, cli::col_green(cli::style_bold(the_tag)))

            # if(the_dataset@is_splitted == TRUE){
            #
            #   the_tag <- "(SMOOTHED, SPLITTED)"
            #
            #   n_train <- sum(the_smoothed_data$train_test == TRUE)
            #   n_test <- sum(the_smoothed_data$train_test == FALSE)
            #
            #   smoothed_and_splitted <- paste0(" [", cli::col_blue(n_train), " train, ",
            #                                   cli::col_blue(n_test), " test]")
            #
            #   the_line <- paste0(base_line, cli::col_green(cli::style_bold(the_tag)), smoothed_and_splitted)
            # }
            # cli::cat_line(the_line)

            cli::cat_line("      ", cli::symbol$en_dash, " ",
                          cli::col_green(the_tag), " ",
                          cli::col_yellow(formatted))

          } else {

            cli::cat_line("      ", cli::symbol$en_dash, " ",
                          cli::col_yellow(formatted))

          }
        }
      }
    }
  }
}

cat_smoothed_data <- function(object){

  if(!is.null(object@smoothed_data)){
    cli::cat_bullet(cli::col_cyan(" Smoothed data : "),
                    pluralize_data_info(object@smoothed_data),
                    bullet = "arrow_right")

    columns_with_smooth <-
      names(which(sapply(colnames(object@smoothed_data),
                         grepl, pattern="_smooth", fixed=TRUE) |
                    sapply(colnames(object@smoothed_data),
                           grepl, pattern="_lag", fixed=TRUE)))

    the_line <-
      paste(cli::symbol$star, "smoothed vars:",
            paste(cli::col_green(sort(columns_with_smooth)),
                  collapse = paste0(" ", cli::symbol$em_dash, " ")))

    cli::cat_line("   ", the_line)
  }

}

cat_discretization_info <- function(object){

  cli::cat_bullet(cli::col_cyan(" Discretized   : "),
                  bullet = "arrow_right")

  cli::cat_line("   ", paste(cli::symbol$star, cli::col_green("Points"),
                             cli::symbol$em_dash,
                             pluralize_data_info(object@points, dim_1_name = "feature")))
  cli::cat_line("   ", paste(cli::symbol$star, cli::col_green("Patches"),
                             cli::symbol$em_dash,
                             pluralize_data_info(object@patches, dim_1_name = "feature")))

}

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

cat_formula <- function(formula, max_length=40){
  formatted <- format_formula(formula)
  if(nchar(formatted)>max_length){
    formatted <- paste0(strtrim(formatted, max_length), "...")
  }
  return(formatted)
}

########

# cat_mapped_datasets <- function(object){
#   datasets <- object@mapped_datasets
#   cli::cli_h3(cli::col_cyan("Mapped Datasets"))
#   cli::cat_bullet(" ", cli::col_cyan(length(datasets)),
#                   " mapped dataset(s): ", paste(cli::col_cyan(sapply(datasets ,
#                                                                      spm_name)),
#                                                 collapse = ", "))
# }

# cat_formulas <- function(object, max_length=70){
#   formulas <- object@formulas
#   cli::cli_h3(cli::col_cyan("Mapped formulas"))
#   for(form_id in seq_len(length.out = length(formulas))){
#
#     the_formula <- formulas[[form_id]]
#     formatted <- format_formula(the_formula@raw_formula)
#     if(nchar(formatted)>max_length){
#       formatted <- paste0(strtrim(formatted, max_length), "...")
#     }
#     cli::cat_line(cli::col_cyan(paste0(form_id, ") ")),
#                   cli::col_yellow(formatted),
#                   " for dataset ", cli::col_cyan(paste0("'", the_formula@dataset, "'")))
#   }
# }

########
