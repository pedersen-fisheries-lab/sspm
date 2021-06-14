#' Create lagged columns in a sspm smoothed data slot
#'
#' This function is a wrapper around [lag][dplyr::lag] (note that not all
#' arguments are supported). The default value for the lag is the mean of the
#' series.
#'
#' @inheritParams map_formula
#' @param vars **\[character\]** Names of the variables to lag.
#' @inheritParams dplyr::lag
#'
#' @export
setGeneric(name = "spm_lag",
           def = function(sspm_object,
                          vars,
                          n = 1,
                          default = "mean",
                          ...){
             standardGeneric("spm_lag")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_lag
setMethod(f = "spm_lag",
          signature(sspm_object = "sspm"),
          function(sspm_object, vars, n, default, ...){

            smoothed_data <- spm_smoothed_data(sspm_object)

            for (var in vars) {

              if(var %in% colnames(smoothed_data)){

                var_name <- paste0(var, "_lag_", n)

                if(is.character(default)){

                  if(default == "mean"){
                    def_val <- mean(smoothed_data[[var]])
                  } else {
                    stop("Defaulting scheme not recognized")
                  }

                } else {
                  def_val <- default
                }

                smoothed_data[[var_name]] <-
                  dplyr::lag(x = smoothed_data[[var]],
                             n = n, default = def_val, ...)
                smoothed_data <- smoothed_data %>%
                  dplyr::relocate(var_name, .after = var)

              } else {

                cli::cli_alert_danger(paste0(" Column ", cli::col_magenta(var),
                                             " does not exist in smoothed data."))

              }
            }

            spm_smoothed_data(sspm_object) <- smoothed_data
            return(sspm_object)
          }
)
