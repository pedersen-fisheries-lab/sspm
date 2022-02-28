# Test discretization step

test_that("Discretization work as expected", {

  discretized <- boundary %>%
    spm_discretize(method = "tesselate_voronoi",
                   with = biomass_dataset,
                   nb_samples = 30)
  expect_class({discretized},
      "sspm_discrete_boundary")

  expect_class({boundary %>%
      spm_discretize(method = "tesselate_voronoi",
                     with = biomass_dataset,
                     nb_samples = c(`4` = 5, `5` = 10, `6` = 7, `7` = 2))},
      "sspm_discrete_boundary")

  expect_class({boundary %>%
      spm_discretize(method = "triangulate_delaunay",
                     with = biomass_dataset)},
      "sspm_discrete_boundary")

  expect_names(names(spm_boundaries(discretized)),
               must.include = c("sfa", "area"))
  expect_names(names(spm_patches(discretized)),
               must.include = c("sfa", "patch_id", "patch_area"))

  expect_equal(dim(spm_patches(discretized))[1], 92)
  expect_equal(dim(spm_patches(discretized))[2], 4)

  expect_equal(dim(spm_points(discretized))[1], 120)
  expect_equal(dim(spm_points(discretized))[2], 10)

  # ----------------------------------------------------------------------

  expect_error({boundary %>%
      spm_discretize(method = "tesselate_voronoi",
                     with = borealis_simulated,
                     nb_samples = 30)},
      "`with` must be a `sspm_dataset` or NULL")

  expect_error({boundary %>%
      spm_discretize(method = "wrong_method",
                     with = biomass_dataset)},
      "Method 'wrong_method' is not part of the supported methods.")

})
