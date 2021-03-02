# Test helper functions
#
# test_that("", {
#
# })

test_that("Methods choices are retrieved", {

  choices <- spm_methods()
  expect_character(choices)
  expect_names(choices, identical.to = "tesselate_voronoi" )

  smooth_choices <- spm_smooth_methods()
  expect_character(smooth_choices)
  expect_names(smooth_choices, identical.to = "ICAR" )

})

test_that("Methods are dispatched correctly", {

  expect_function(dispatch_method("tesselate_voronoi"))
  expect_message(dispatch_method("method_not_supported"),
                 "Method 'method_not_supported' is not part of the supported methods.")

  expect_function(dispatch_smooth("ICAR"))
  expect_message(dispatch_smooth("method_not_supported"),
                 "Smoothing method 'method_not_supported' is not part of the supported methods.")

})
