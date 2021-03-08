# Test of map_formula

test_that("Map formula works as expected", {

  form_mapped <- sspm_discrete_mapped %>%
    map_formula("Biomass", weight_per_km2~smooth_time()+smooth_space()+smooth_space_time())
  the_form <- translated_formula(spm_mapped_formulas(form_mapped)[[1]])

  expect_formula(the_form)
  expect_match(sspm:::format_formula(the_form), fixed = TRUE,
               paste0("weight_per_km2 ~ s(year_f, k = 24L, bs = 're',",
                      " xt = list(penalty = pen_mat_time)) + ",
                      "s(patch_id, k = 30, bs = 'mrf', ",
                      "xt = list(penalty = pen_mat_space)) + ",
                      "ti(year_f, patch_id, k = c(24, 30), bs = c('re', 'mrf'), ",
                      "xt = list(year_f = list(penalty = pen_mat_time), ",
                      "patch_id = list(penalty = pen_mat_space)))"))

  # Error cases
  expect_message({
    sspm_discrete_mapped %>% map_formula("Biomass")
  }, "Argument 'formula' missing with no default")

})
