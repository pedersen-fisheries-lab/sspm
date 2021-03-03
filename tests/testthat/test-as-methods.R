# Testing as_object method

test_that("Discret method is casted correctly", {

  method <- "tesselate_voronoi"
  expect_class(as_discretization_method(method), "discretization_method")

  method <- "method_not_supported"
  expect_message(as_discretization_method(method),
                 "Method 'method_not_supported' is not part of the supported methods.")

})
