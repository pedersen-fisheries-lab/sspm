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
            cat_boundaries(object)
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_discrete_boundary",
          function(object) {
            cli::cat_line()
            custom_h1(paste0("SSPM Boundary ", cli::col_green("(Discrete)")))
            cat_boundaries(object)
            cat_discretization_info(object)
            cli::cat_line()
          }
)

setMethod("show",
          "sspm_data",
          function(object) {
            cli::cat_line()
            custom_h1(paste0("SSPM Dataset: ", cli::col_blue(object@name)))
            cli::cat_bullet(" Data              : ",
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

  cli::cat_bullet(" Boundaries    : ",
                  pluralize_data_info(object@boundaries),
                  bullet = "arrow_right")
  cli::cat_bullet(" Boundary col. : ",
                  cli::col_blue(object@boundary_column),
                  bullet = "arrow_right")

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

        cat_formula_lines(the_dataset, the_dataset_formulas)

      }
    }
  }
}

cat_formula_lines <- function(the_dataset, the_dataset_formulas){

  for (f_id in seq_len(length.out = length(the_dataset_formulas))){

    form <- the_dataset_formulas[[f_id]]
    formatted <- cat_formula(form@raw_formula)

    if(the_dataset@is_smoothed == TRUE){

      the_tag <- "(SMOOTHED)"

      cli::cat_line("      ", cli::symbol$en_dash, " ",
                    cli::col_green(the_tag), " ",
                    cli::col_yellow(formatted))

    } else {

      cli::cat_line("      ", cli::symbol$en_dash, " ",
                    cli::col_yellow(formatted))

    }
  }

}

cat_smoothed_data <- function(object){

  if(!is.null(object@smoothed_data@data)){
    cli::cat_bullet(cli::col_cyan(" Smoothed data : "),
                    pluralize_data_info(object@smoothed_data@data),
                    bullet = "arrow_right")

    columns_with_smooth <-
      names(which(sapply(colnames(object@smoothed_data@data),
                         grepl, pattern = "_smooth", fixed=TRUE)))
    columns_with_smooth <-
      names(which(!sapply(columns_with_smooth,
                          grepl, pattern = "lag", fixed=TRUE)))
    columns_with_catch <-
      names(which(sapply(colnames(object@smoothed_data@data),
                         grepl, pattern = "_with_catch", fixed=TRUE)))

    if(length(columns_with_catch) > 0){
      columns_with_smooth <- columns_with_smooth[!(columns_with_smooth %in%
                                                     columns_with_catch)]
    }

    the_line <-
      paste(cli::symbol$star, "smoothed vars:",
            paste(cli::col_green(sort(columns_with_smooth)),
                  collapse = paste0(" ", cli::symbol$em_dash, " ")))

    cli::cat_line("   ", the_line)

    if(length(columns_with_catch) > 0){
      the_line <-
        paste(cli::symbol$star, "smoothed vars with catch:",
              paste(cli::col_green(sort(columns_with_catch)),
                    collapse = paste0(" ", cli::symbol$em_dash, " ")))

      cli::cat_line("   ", the_line)
    }

    columns_with_lag <-
      names(which(sapply(colnames(object@smoothed_data@data),
                         grepl, pattern="_lag", fixed=TRUE)))

    if (length(columns_with_lag) > 0){
      the_line <-
        paste(cli::symbol$star, "lagged vars:",
              paste(cli::col_green(sort(columns_with_lag)),
                    collapse = paste0(" ", cli::symbol$em_dash, " ")))

      cli::cat_line("   ", the_line)
    }


    cat_formula_lines(object@smoothed_data, object@smoothed_data@formulas)

    if(is_splitted(object@smoothed_data)){

      n_train <- sum(object@smoothed_data@data$train_test)
      n_test <- sum(!object@smoothed_data@data$train_test)

      split_info <- paste0("[", cli::col_blue(n_train),
                           cli::col_yellow(" train, "),
                           cli::col_blue(n_test),
                           cli::col_yellow(" test"), "]")
      cli::cat_line("      ", cli::symbol$en_dash, " ",
                    split_info)

    }

  }

  if(length(object@smoothed_data@smoothed_fit) > 0){
    cli::cat_bullet(cli::col_green(" SPM FITTED"),
                    bullet = "tick")
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

cat_formula <- function(formula, max_length=40){
  formatted <- format_formula(formula)
  if(nchar(formatted)>max_length){
    formatted <- paste0(strtrim(formatted, max_length), "...")
  }
  return(formatted)
}
