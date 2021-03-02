# Test accessors
#
# test_that("Accessors work as expected on ``", {
#
# })

test_that("Accessors work as expected on `spaspm_data`", {

  # Valid
  expect_class(spm_name(spaspm_data), "character")

  expect_data_frame(spm_data(spaspm_data))

  expect_character(spm_rep(spaspm_data))
  expect_length(spm_rep(spaspm_data), 1)

  expect_character(spm_unique_ID(spaspm_data))
  expect_length(spm_unique_ID(spaspm_data), 1)

  expect_character(spm_coords_col(spaspm_data))
  expect_length(spm_coords_col(spaspm_data), 2)

  expect_character(spm_time_col(spaspm_data))
  expect_length(spm_time_col(spaspm_data), 1)

  # Invalid
  expect_message(spm_datasets(spaspm_data),
                 "Use `spm_data` to access the data of a dataset object")

})

test_that("Accessors work as expected on `spaspm`", {

  # Valid
  expect_character(spm_name(spaspm_base))

  expect_character(spm_unique_ID(spaspm_base))
  expect_length(spm_unique_ID(spaspm_base), 1)

  expect_character(spm_coords_col(spaspm_base))
  expect_length(spm_coords_col(spaspm_base), 2)

  expect_character(spm_time_col(spaspm_base))
  expect_length(spm_time_col(spaspm_base), 1)

  expect_class(spm_boundaries(spaspm_base), "sf")

  expect_class(spm_base_dataset(spaspm_base), "spaspm_data")

  expect_class(spm_datasets(spaspm_base), "spaspm_data")
  expect_length(spm_datasets(spaspm_discrete), 1)

  # Not valid
  expect_message(spm_data(spaspm_base),
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")
  expect_message(spm_discret_method(spaspm_base),
                 "Model object 'Model test' is not a discrete model",)
  expect_message(spm_patches(spaspm_base),
                 "Model object 'Model test' is not a discrete model")
  expect_message(spm_points(spaspm_base),
                 "Model object 'Model test' is not a discrete model")
  expect_message(spm_mapped_datasets(spaspm_base),
                 "Model object 'Model test' is not a discrete model")
  expect_message(spm_mapped_formulas(spaspm_base),
                 "Model object 'Model test' is not a discrete model")

})

test_that("accessors work on `spaspm_discrete`", {

  # Valid
  expect_character(spm_name(spaspm_discrete))

  expect_character(spm_unique_ID(spaspm_discrete))
  expect_length(spm_unique_ID(spaspm_discrete), 1)

  expect_character(spm_coords_col(spaspm_discrete))
  expect_length(spm_coords_col(spaspm_discrete), 2)

  expect_character(spm_time_col(spaspm_discrete))
  expect_length(spm_time_col(spaspm_discrete), 1)

  expect_class(spm_boundaries(spaspm_discrete), "sf")

  expect_class(spm_base_dataset(spaspm_discrete), "spaspm_data")

  expect_list(spm_datasets(spaspm_discrete))
  expect_length(spm_datasets(spaspm_discrete), 1)

  expect_class(spm_discret_method(spaspm_discrete), "discretization_method")

  expect_class(spm_patches(spaspm_discrete),"sf")
  expect_class(spm_points(spaspm_discrete), "sf")

  expect_list(spm_mapped_datasets(spaspm_discrete))
  expect_length(spm_mapped_datasets(spaspm_discrete), 0)

  expect_list(spm_mapped_formulas(spaspm_discrete))
  expect_length(spm_mapped_formulas(spaspm_discrete), 0)

  # Not valid
  expect_message(spm_data(spaspm_discrete),
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")
})

test_that("Accessors work as expected on `spaspm_discrete (MAPPED)`", {

  # Valid
  expect_character(spm_name(spaspm_discrete_mapped))

  expect_class(spm_unique_ID(spaspm_discrete_mapped), "list")
  expect_length(spm_unique_ID(spaspm_discrete_mapped), 2)

  expect_list(spm_coords_col(spaspm_discrete_mapped))
  expect_length(spm_coords_col(spaspm_discrete_mapped), 2)

  expect_list(spm_time_col(spaspm_discrete_mapped))
  expect_length(spm_time_col(spaspm_discrete_mapped), 2)

  expect_class(spm_boundaries(spaspm_discrete_mapped), "sf")

  expect_class(spm_base_dataset(spaspm_discrete_mapped), "spaspm_data")

  expect_list(spm_datasets(spaspm_discrete_mapped))
  expect_length(spm_datasets(spaspm_discrete_mapped), 2)

  expect_class(spm_discret_method(spaspm_discrete_mapped),
               "discretization_method")

  expect_class(spm_patches(spaspm_discrete_mapped),"sf")
  expect_class(spm_points(spaspm_discrete_mapped), "sf")

  expect_list(spm_mapped_datasets(spaspm_discrete_mapped))
  expect_length(spm_mapped_datasets(spaspm_discrete_mapped), 1)

  expect_list(spm_mapped_formulas(spaspm_discrete_mapped))
  expect_length(spm_mapped_formulas(spaspm_discrete_mapped), 0)

  # Not valid
  expect_message(spm_data(spaspm_discrete_mapped),
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")

})

test_that("Accessors work as expected on `spaspm_discrete (MAPPED + FORMULAS)`", {

  expect_list(spm_mapped_formulas(spaspm_discrete_mapped_forms))
  expect_length(spm_mapped_formulas(spaspm_discrete_mapped_forms), 1)
  expect_class(spm_mapped_formulas(spaspm_discrete_mapped_forms)[[1]],
               "spaspm_formula")

})

test_that("Accesors work as expected on `discretization_method`"{

  expect_character(spm_name(discret_method))
  expect_class(method_func(discret_method), "function")

})

test_that("Accesors work as expected on `spaspm_formula`"{

  expect_class(raw_formula(spaspm_formula), "formula")
  expect_class(translated_formula(spaspm_formula), "formula")
  expect_character(dataset(spaspm_formula))
  expect_list(formula_vars(spaspm_formula))

})
