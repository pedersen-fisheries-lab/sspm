# Testing as_object method

test_that("Discret method is casted correctly", {

  method <- "tesselate_voronoi"
  expect_class(as_discretization_method(method), "discretization_method")

  method <- "method_not_supported"
  expect_message(as_discretization_method(method),
                 "Method 'method_not_supported' is not part of the supported methods.")

})

test_that("sspm data is casted correctly", {

  # Test the 3 generic cases
  expect_error({
    spm_as_dataset(data = borealis_simulated,
                 time_column = "year_f",
                 coords = c('lon_dec','lat_dec'),
                 name = "Biomass",
                 uniqueID = "Bad column")
  }, "`uniqueID` must be a column of `data`")

  borealis_simulated_NU <- borealis_simulated
  borealis_simulated_NU$new_col <- "Non_unique"
  expect_error({
    spm_as_dataset(data = borealis_simulated_NU,
                 type = "biomass",
                 time_column = "year_f",
                 coords = c('lon_dec','lat_dec'),
                 name = "Biomass",
                 uniqueID = "new_col")
  }, "`uniqueID` must be unique for each row of `data`")

  expect_error({
    spm_as_dataset(data = borealis_simulated,
                 type = "biomass",
                 time_column = "Bad column",
                 coords = c('lon_dec','lat_dec'),
                 name = "Biomass",
                 uniqueID = "uniqueID")
  }, "`time_column` must be a column of `data`")

  # If data matrix is df, coords must be provided
  expect_error({
    spm_as_dataset(data = borealis_simulated,
                 type = "biomass",
                 time_column = "year_f",
                 name = "Biomass",
                 uniqueID = "uniqueID")
  }, "Argument `coords` must be provided when data matrix is a dataframe")

  # Coords must be columns of data
  expect_error({
    spm_as_dataset(data = borealis_simulated,
                 type = "biomass",
                 time_column = "year_f",
                 coords = c('Bad column 1','Bad column 2'),
                 name = "Biomass",
                 uniqueID = "uniqueID")
  }, "`coords` must be columns of `data`")

  # When works fine
  expect_class({
    spm_as_dataset(data = borealis_simulated,
                 type = "biomass",
                 time_column = "year_f",
                 coords = c('lon_dec','lat_dec'),
                 name = "Biomass",
                 uniqueID = "uniqueID")
  }, "sspm_data")

  expect_class({
    spm_as_dataset(data = borealis_simulated,
                 type = "biomass",
                 time_column = "year_f",
                 coords = list('lon_dec','lat_dec'),
                 name = "Biomass",
                 uniqueID = "uniqueID")
  }, "sspm_data")

  expect_class({
    spm_as_dataset(data = borealis_spatial,
                 type = "biomass",
                 time_column = "year_f",
                 coords = c('lon_dec','lat_dec'),
                 name = "Biomass",
                 uniqueID = "uniqueID")
  }, "sspm_data")

})
