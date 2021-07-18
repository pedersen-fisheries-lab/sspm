# Test of map_dataset

test_that("Mapping of dataset works as expected", {
  skip("Deprecated")

  expect_error({
    sspm_discrete %>% map_dataset(predator_simulated, name = "pred_data", time_column = "year",
                                  uniqueID = "uniqueID", coords = c("lat_dec"))
  }, "`coords` must be of length 2")

  # Success cases
  mapped <- sspm_discrete %>%
    map_dataset(predator_simulated, name = "pred_data", time_column = "year",
                uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  expect_length(spm_datasets(mapped), 2)

  mapped_from_df <- sspm_discrete %>%
    map_dataset(predator_simulated, name = "pred_data", time_column = "year",
                uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  expect_length(spm_datasets(mapped_from_df), 2)

  # Test list
  # Error cases
  dataset_list <- list(predator_simulated, predator_simulated)
  expect_error({
    sspm_discrete %>%
      map_dataset(dataset_list, name = "pred_data", time_column = "year",
                  uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  }, "Argument 'name' is of length 1, but should be a unique set of the same length than data.")

  expect_error({
    sspm_discrete %>%
      map_dataset(dataset_list, name = c("pred_data", "pred_data"), time_column = "year",
                  uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  }, "Argument 'name' is of length 2, but should be a unique set of the same length than data.")

  # Coords 3 cases
  expect_error({
    sspm_discrete %>%
      map_dataset(dataset_list, name = c("pred_data", "pred_data_2"),
                  time_column = "year",
                  uniqueID = "uniqueID", coords = list(c("lon_dec", "lat_dec"),
                                                       c("lon_dec", "lat_dec"),
                                                       c("lon_dec", "lat_dec")))

  }, "Argument 'coords' should be a list of the same length than data.")

  expect_error({
    sspm_discrete %>%
      map_dataset(list(predator_simulated), name = c("pred_data"), time_column = "year",
                  uniqueID = "uniqueID", coords = c("lat_dec"))
  }, "Argument 'coords' should be of a vector of length 2.")

  expect_error({
    sspm_discrete %>%
      map_dataset(dataset_list, name = c("pred_data", "pred_data_2"), time_column = "year",
                  uniqueID = "uniqueID", coords = list(c("lon_dec", "lat_dec"),
                                                                           c("lon_dec")))
  }, "Argument 'coords' should be a list of elements of length 2.")

  # One arg too long
  expect_error({
    sspm_discrete %>%
      map_dataset(dataset_list, name = c("pred_data", "pred_data_2"),
                  time_column = c("year", "year", "year"),
                  uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  }, "Argument 'time_column' is of length 3. Must be of length 1 or of the same length than data.")

  # Success cases
  # Two datasets
  expect_class({
    sspm_discrete %>%
      map_dataset(dataset_list, name = c("pred_data", "pred_data_2"), time_column = "year",
                  uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  }, "sspm_discrete")

  # One dataset
  expect_class({
    sspm_discrete %>%
      map_dataset(list(predator_simulated), name = "pred_data", time_column = "year",
                  uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
  }, "sspm_discrete")

})
