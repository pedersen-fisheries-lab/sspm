# Test of map_dataset

test_that("Mapping of dataset works as expected", {

  expect_message(sspm_base %>% map_dataset(predator_spatial),
                 "Model object 'Model test' is not a discrete model")

  expect_length(spm_mapped_datasets(sspm_discrete), 0)
  mapped <- sspm_discrete %>%
    map_dataset(predator_sim, name = "pred_data", time_col = "year",
                uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  expect_length(spm_mapped_datasets(mapped), 1)

  mapped_from_df <- sspm_discrete %>%
    map_dataset(predator_simulated, name = "pred_data", time_col = "year",
                uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  expect_length(spm_mapped_datasets(mapped_from_df), 1)

})
