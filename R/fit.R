#' Fit the GAM part of a sspm model
#'
#' Once formulas have been mapped onto a sspm discrete object, the GAMs can be
#' fitted with this function. Arguments can be passed onto `bam`.
#'
#' @param sspm_object **\[sspm_discrete\]** An object of class
#'   [sspm_discrete][sspm_discrete-class].
#' @inheritParams mgcv::bam
#' @inheritDotParams mgcv::bam
#'
#' @rdname fit
#' @export
setGeneric(name = "fit_smooths",
           def = function(sspm_object,
                          family = mgcv::tw,
                          drop.unused.levels = F,
                          method = "fREML",
                          ...){
             standardGeneric("fit_smooths")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname fit
setMethod(f = "fit_smooths",
          signature(sspm_object = "sspm"),
          function(sspm_object, ...){
            message_not_discrete(sspm_object)
          }
)

#' @export
#' @rdname fit
setMethod(f = "fit_smooths",
          signature(sspm_object = "sspm_discrete"),
          function(sspm_object, family, drop.unused.levels, method, ...){

            # Get all datasets
            datasets <- spm_datasets(sspm_object)

            # has_formulas <- sapply(datasets, function(x){
            #   length(spm_formulas(x)) > 0
            # })

            for(dataset in datasets){

              if(!is_smoothed(dataset)){

                formulas <- spm_formulas(dataset)
                formula_length <- length(formulas)

                if(formula_length == 0){
                  next
                } else{

                  tmp_fit <-
                    vector(mode = "list", length = sum(formula_length))
                  tmp_smoothed <-
                    vector(mode = "list", length = sum(formula_length))

                  for (form_id in seq_len(length.out = length(formulas))){

                    # Index formula
                    form <- formulas[[form_id]]
                    form_name <- paste0(spm_name(dataset), "_f", form_id)

                    # Inject name
                    names(tmp_fit)[form_id] <- form_name
                    names(tmp_smoothed)[form_id] <- form_name

                    # Print info
                    cli::cli_alert_info(
                      paste0(" Fitting formula: ",
                             cli::col_yellow(format_formula(raw_formula(form))),
                             " for dataset ", cli::col_cyan(paste0("'", spm_name(dataset),"'"))))

                    # Get data
                    the_data <- spm_data(dataset)
                    form_vars <- formula_vars(form)

                    # Modify formula env, best solution for now
                    form_env <- attr(translated_formula(form), ".Environment")
                    for(var in names(form_vars)){
                      assign(x = var, value = form_vars[[var]], envir = form_env)
                    }

                    # Fit the formula, important to attach the vars
                    tmp_fit[[form_name]] <-
                      mgcv::bam(formula = translated_formula(form),
                                data = the_data,
                                family = family,
                                drop.unused.levels = drop.unused.levels,
                                method = method,
                                ...)

                    tmp_df <- data.frame(tmp_fit[[form_name]]$fitted)
                    names(tmp_df) <- form_name
                    tmp_smoothed[[form_name]] <- tmp_df
                  }
                  spm_smoothed_data(datasets[[spm_name(dataset)]]) <- tmp_smoothed
                  spm_smoothed_fit(datasets[[spm_name(dataset)]]) <- tmp_fit
                }
                is_smoothed(datasets[[spm_name(dataset)]]) <- TRUE
              }
            }

            spm_datasets(sspm_object) <- datasets

            # For now return a summary of the fit
            return(sspm_object)

          }
)
