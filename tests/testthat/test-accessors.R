# Test accessors
#
# test_that("Accessors work as expected on ``", {
#
# })

test_that("Accessors work as expected on `sspm_dataset` (smoothed or not)", {

  # Valid

  expect_data_frame(spm_data(biomass_dataset))
  expect_data_frame(spm_data(biomass_dataset_smoothed))

  expect_character(spm_unique_ID(biomass_dataset))
  expect_length(spm_unique_ID(biomass_dataset), 1)

  expect_character(spm_unique_ID(biomass_dataset_smoothed))
  expect_length(spm_unique_ID(biomass_dataset_smoothed), 1)

  expect_character(spm_coords_col(biomass_dataset))
  expect_length(spm_coords_col(biomass_dataset), 2)

  expect_character(spm_coords_col(biomass_dataset_smoothed))
  expect_length(spm_coords_col(biomass_dataset_smoothed), 2)

  expect_character(spm_time_column(biomass_dataset))
  expect_length(spm_time_column(biomass_dataset), 1)

  expect_character(spm_time_column(biomass_dataset_smoothed))
  expect_length(spm_time_column(biomass_dataset_smoothed), 1)

  expect_class(spm_boundaries(biomass_dataset), "sspm_boundary")
  expect_class(spm_boundaries(biomass_dataset_smoothed), "sspm_discrete_boundary")

  expect_null(spm_smoothed_data(biomass_dataset))
  expect_data_frame(spm_smoothed_data(biomass_dataset_smoothed))

  expect_length(spm_formulas(biomass_dataset), 0)
  expect_list(spm_formulas(biomass_dataset_smoothed))
  expect_length(spm_formulas(biomass_dataset_smoothed), 1)

})

test_that("Accessors work as expected on `sspm`", {

  expect_class(spm_boundaries(sspm_model), "sspm_discrete_boundary")

  expect_list(spm_datasets(sspm_model))
  expect_length(spm_datasets(sspm_model), 3)

  expect_data_frame(spm_smoothed_data(sspm_model))

  expect_class(spm_boundaries(sspm_model), "sspm_discrete_boundary")

})

test_that("Accesors work as expected on `discretization_method`", {

  expect_character(spm_name(discret_method))
  expect_function(method_func(discret_method))

})

test_that("Accesors work as expected on `sspm_formula`", {

  expect_formula(raw_formula(sspm_formula))
  expect_formula(translated_formula(sspm_formula))
  expect_list(formula_vars(sspm_formula))

})
