# Test discretization step

test_that("Discretization work as expected", {
  skip("TODO")

  # If error
  expect_error({sspm_base %>%
      spm_discretize(with_dataset = "Biomass",
                     discretization_method = "invalid method")},
      "Invalid discretization method.")

  # If success
  discretized <- sspm_base %>%
    spm_discretize(with_dataset = "Biomass",
                   discretization_method = "tesselate_voronoi")
  expect_class({discretized}, "sspm_discrete")

  expect_names(names(spm_data(spm_datasets(discretized)$Biomass)),
               must.include = c("patch_id", "area_km2"))

  expect_equal(dim(spm_data(spm_datasets(discretized)$Biomass))[1], 1026)
  expect_equal(dim(spm_data(spm_datasets(discretized)$Biomass))[2], 21)

  expect_equal(dim(spm_patches(discretized))[1], 69)
  expect_equal(dim(spm_patches(discretized))[2], 4)

  expect_equal(dim(spm_points(discretized))[1], 75)
  expect_equal(dim(spm_points(discretized))[2], 19)

  expect_message({discretized %>%
      spm_discretize(discretization_method = "tesselate_voronoi")},
      "Model 'Model test' is already discretized")

  expect_message({discretized %>%
    spm_discretize(discretization_method = "tesselate_voronoi")},
    "Model 'Model test' is already discretized")

})
