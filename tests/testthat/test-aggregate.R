# Test aggregation

test_that("Aggregation works as expected", {

  expect_class({
    spm_aggregate(biomass_dataset_smoothed, variable = "weight_per_km2",
                  fun = mean, type = "smoothed", group_by = "time")
  }, "sspm_dataset")

  expect_class({
    spm_aggregate(biomass_dataset_smoothed, variable = "weight_per_km2",
                  fun = mean, type = "smoothed", group_by = "space")
  }, "sspm_dataset")

})
