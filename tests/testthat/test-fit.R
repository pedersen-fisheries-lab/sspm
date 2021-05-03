# Test fit function

test_that("Fitting works as expected", {
  sspm_discrete <- sspm_base %>%
    spm_discretize(with_dataset = "Biomass",
                   discretization_method = "tesselate_voronoi")
  sspm_discrete_mapped_forms <- sspm_discrete %>%
    map_formula(weight_per_km2~smooth_space(), "Biomass")
  sspm_discrete_mapped_fitted <- sspm_discrete_mapped_forms %>%
    fit_smooths()
  intercept <-
    sspm_discrete_mapped_fitted@datasets$Biomass@smoothed_fit[[1]]$coefficients[[1]]
  expect_equal(intercept, 8.504188, tolerance = 1e-5)
})
