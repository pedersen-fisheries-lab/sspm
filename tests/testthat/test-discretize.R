# Test discretization step

test_that("Discretization work as expected", {

  # If error
  expect_error({spaspm_base %>%
      spm_discretize(discretization_method = "invalid method")},
      "Invalid discretization method.")

  # If success
  discretized <- spaspm_base %>%
    spm_discretize(discretization_method = "tesselate_voronoi")
  expect_class({discretized},"spaspm_discrete")

  expect_names(names(spm_data(spm_base_dataset(discretized))),
               must.include = c("patch_id", "area_km2"))

  expect_equal(dim(spm_data(spm_base_dataset(discretized)))[1], 1541)
  expect_equal(dim(spm_data(spm_base_dataset(discretized)))[2], 21)

  expect_equal(dim(spm_patches(discretized))[1], 69)
  expect_equal(dim(spm_patches(discretized))[2], 4)

  expect_equal(dim(spm_points(discretized))[1], 75)
  expect_equal(dim(spm_points(discretized))[2], 19)

  expect_message({discretized %>%
      spm_discretize(discretization_method = "tesselate_voronoi")},
      "Model 'Model test' is already discretized")

  # SAME TESTS for when discretized over again
  discretized_twice <- discretized %>%
    spm_discretize(discretization_method = "tesselate_voronoi", force = TRUE)
  expect_class({discretized},"spaspm_discrete")

  expect_names(names(spm_data(spm_base_dataset(discretized_twice))),
               must.include = c("patch_id", "area_km2"))

  expect_equal(dim(spm_data(spm_base_dataset(discretized_twice)))[1], 1541)
  expect_equal(dim(spm_data(spm_base_dataset(discretized_twice)))[2], 21)

  expect_equal(dim(spm_patches(discretized_twice))[1], 69)
  expect_equal(dim(spm_patches(discretized_twice))[2], 4)

  expect_equal(dim(spm_points(discretized_twice))[1], 75)
  expect_equal(dim(spm_points(discretized_twice))[2], 19)

})
