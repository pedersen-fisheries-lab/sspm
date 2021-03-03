# Test spaspm function

test_that("Spaspm object are created correcly", {

  expect_class( {
    spaspm(model_name = "My Model",
           data = borealis_simulated,
           time_col = "year_f",
           coords = c('lon_dec','lat_dec'),
           uniqueID = "uniqueID",
           boundaries = sfa_boundaries) },
    "spaspm")

})
