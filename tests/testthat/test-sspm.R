# Test sspm function

test_that("sspm object are created correcly", {

  expect_class( {
    sspm(model_name = "My Model",
         boundaries = sfa_boundaries) },
    "sspm")

})
