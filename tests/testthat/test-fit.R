# Test fit function

test_that("Fitting works fas expected", {
  sspm_discrete <- sspm_base %>%
    spm_discretize(discretization_method = "tesselate_voronoi")
  sspm_discrete_mapped_forms <- sspm_discrete %>%
    map_formula("Biomass", weight_per_km2~smooth_space())
  sspm_discrete_mapped_fitted <- sspm_discrete_mapped_forms %>%
    fit_smooths()
  print(as.numeric(sspm_discrete_mapped_fitted[[1]]$p.coeff))
  expect_equal(as.numeric(sspm_discrete_mapped_fitted[[1]]$p.coeff), 8.504188,
               tolerance = 1e-5)
})
