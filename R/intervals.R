
# Functions for making intervals from bam objects -------------------------

# Expect a gam fit object, returns a data.frame

produce_sims <- function(fit, new_data, n = 1000){

  checkmate::assert_class(fit, "gam")

  coefs <- stats::coef(fit)
  lp <- predict(fit, newdata = new_data, type = "lpmatrix")
  vcv <- stats::vcov(fit)
  coefs_sim <- t(rmvn(n = n, coefs, vcv))

  sims <- lp %*% coefs_sim

  return(sims)

}

find_quantiles <- function(mat, prob = c(0.025, 0.975), name, MARGIN = 1,
                           na.rm = TRUE, compute_exp = TRUE) {

  quants <- t(apply(mat, MARGIN = MARGIN, FUN = stats::quantile,
                    prob = prob, na.rm = na.rm)) %>%
    as.data.frame()

  colnames(quants) <-  paste0(name, c("_log_lower", "_log_upper"))

  if (compute_exp) {

    quants_exp <- exp(quants)
    colnames(quants_exp) <-  paste0(name, c("_lower", "_upper"))
    quants <- dplyr::bind_cols(quants, quants_exp)

  }

  return(quants)
}

confidence_interval <- function(sims, ...){

  CI <- find_quantiles(sims, name = "CI", ...)

  return(CI)
}

prediction_interval <- function(fit, sims, ...){

  checkmate::assert_class(fit, "gam")

  model_family <- fit$family
  invlink <- model_family$linkinv
  inv_sims <- invlink(sims)

  fam_char <- as.character(model_family)[1]

  if (grepl("Scaled", fam_char)){

    min_df <- environment(model_family[["rd"]])[[".min.df"]]
    theta <- environment(model_family[["rd"]])[[".Theta"]]

    nu <- min_df + exp(theta[1])
    sig <- exp(theta[2])

    pred_sims <- sig*(matrix(stats::rt(prod(dim(inv_sims)), df = nu),
                             nrow = nrow(inv_sims),
                             ncol = ncol(inv_sims))) + inv_sims

  } else if (grepl("Tweedie", fam_char)){

    p_char <- gsub(gsub(fam_char, pattern = "Tweedie(p=",
                        replacement = "", fixed = T), pattern = ")",
                   replacement = "", fixed = TRUE)
    p <- as.numeric(p_char)

    pred_sims <- matrix(rTweedie(mu = c(inv_sims), p = p),
                        nrow = nrow(inv_sims),
                        ncol = ncol(inv_sims))

  } else {

    stop("PI is only supported for sspm_fit if the family is scat")

  }

  PI <- find_quantiles(pred_sims, name = "PI", ...)

  return(PI)
}
