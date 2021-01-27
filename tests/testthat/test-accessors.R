# Class construction
test_that("spaspm object is created correctly", {
  expect_class({spaspm_base <- new("spaspm",
                                   name="Model test",
                                   data=borealis,
                                   boundaries=sfa)}, "spaspm")
})

test_that("spaspm_discrete object is created correctly", {
  expect_class({spaspm_base <- new("spaspm_discrete",
                                   name = spm_name(spaspm_base),
                                   data = spm_data(spaspm_base),
                                   boundaries = spm_boundaries(spaspm_base),
                                   data_spatial = borealis_spatial,
                                   method = the_method,
                                   patches = the_patches,
                                   points = the_points)}, "spaspm_discrete")
})

# Test accessors
test_that("accessors work on spaspm", {

  expect_class({spm_name(spaspm_base)}, "character")
  expect_class({spm_data(spaspm_base)}, "data.frame")
  expect_class({spm_boundaries(spaspm_base)}, "sf")

  expect_message({spm_discret_method(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")
  expect_message({spm_patches(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")
  expect_message({spm_points(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")
})

test_that("accessors work on spaspm_discrete", {

  expect_class({spm_name(spaspm_discrete)}, "character")
  expect_class({spm_data(spaspm_discrete)}, "list")
  expect_class({spm_boundaries(spaspm_discrete)}, "sf")

  expect_class({spm_discret_method(spaspm_discrete)}, "discretization_method")
  expect_class({spm_patches(spaspm_discrete)}, "sf")
  expect_class({spm_points(spaspm_discrete)}, "sf")
})


