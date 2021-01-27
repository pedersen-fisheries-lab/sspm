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
