# Test discretization step

test_that("Discretization work as expected", {

  # TODO test the dimension of object, the presence of new column, patches and points, etc...
  discretized <- spaspm_base %>%
    spm_discretize(discretization_method = "tesselate_voronoi")
  expect_class({discretized},"spaspm_discrete")

  expect_message({discretized %>%
      spm_discretize(discretization_method = "tesselate_voronoi")},
      "Model 'Model test' is already discretized")

  discretized_twice <- discretized %>%
    spm_discretize(discretization_method = "tesselate_voronoi", force = TRUE)
  expect_class({discretized},"spaspm_discrete")

})
