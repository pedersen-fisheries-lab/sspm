# Test sspm function

test_that("sspm object are created correcly", {

  expect_class( {
    sspm(model_name = "My Model",
         data = borealis_simulated,
         time_col = "year_f",
         coords = c('lon_dec','lat_dec'),
         uniqueID = "uniqueID",
         boundaries = sfa_boundaries) },
    "sspm")

})
