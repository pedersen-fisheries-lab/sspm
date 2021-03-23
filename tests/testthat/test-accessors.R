# Test accessors
#
# test_that("Accessors work as expected on ``", {
#
# })

test_that("Accessors work as expected on `sspm_data`", {

  # Valid
  expect_character(spm_name(sspm_data))

  expect_data_frame(spm_data(sspm_data))

  expect_character(spm_unique_ID(sspm_data))
  expect_length(spm_unique_ID(sspm_data), 1)

  expect_character(spm_coords_col(sspm_data))
  expect_length(spm_coords_col(sspm_data), 2)

  expect_character(spm_time_column(sspm_data))
  expect_length(spm_time_column(sspm_data), 1)

  expect_list(spm_formulas(sspm_data))
  expect_length(spm_formulas(sspm_data), 0)

  # Invalid
  expect_message(spm_datasets(sspm_data),
                 "Use `spm_data` to access the data of a dataset object")

})

test_that("Accessors work as expected on `sspm`", {

  # Valid
  expect_character(spm_name(sspm_base))
  expect_match(spm_name(sspm_base), "Model test")

  expect_class(spm_boundaries(sspm_base), "sf")

  expect_list(spm_datasets(sspm_base))
  expect_length(spm_datasets(sspm_discrete), 0)

  # Not valid
  expect_message(spm_data(sspm_base),
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a sspm object")
  expect_message(spm_discret_method(sspm_base),
                 "Model object 'Model test' is not a discrete model",)
  expect_message(spm_patches(sspm_base),
                 "Model object 'Model test' is not a discrete model")
  expect_message(spm_points(sspm_base),
                 "Model object 'Model test' is not a discrete model")

})

test_that("accessors work on `sspm_discrete`", {

  # Valid
  expect_character(spm_name(sspm_discrete))
  expect_match(spm_name(sspm_base), "Model test")

  expect_class(spm_boundaries(sspm_discrete), "sf")

  expect_list(spm_datasets(sspm_discrete))
  expect_length(spm_datasets(sspm_discrete), 0)

  expect_class(spm_discret_method(sspm_discrete), "discretization_method")

  expect_class(spm_patches(sspm_discrete),"sf")
  expect_class(spm_points(sspm_discrete), "sf")

  # Not valid
  expect_message(spm_data(sspm_discrete),
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a sspm object")
})

test_that("Accessors work as expected on `sspm_discrete (MAPPED)`", {

  # Valid
  expect_character(spm_name(sspm_discrete_mapped))
  expect_match(spm_name(sspm_base), "Model test")

  expect_class(spm_boundaries(sspm_discrete_mapped), "sf")

  expect_list(spm_datasets(sspm_discrete_mapped))
  expect_length(spm_datasets(sspm_discrete_mapped), 2)

  expect_class(spm_discret_method(sspm_discrete_mapped),
               "discretization_method")

  expect_class(spm_patches(sspm_discrete_mapped),"sf")
  expect_class(spm_points(sspm_discrete_mapped), "sf")

  # Not valid
  expect_message(spm_data(sspm_discrete_mapped),
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a sspm object")

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
