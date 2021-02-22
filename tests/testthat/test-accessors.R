# Test accessors
#
# For mosts: name, base_dataset, datasets, data, coords_col, discret_method,
# mapped_datasets, unique_ID, patches, points
# For method: method_func
#
# test_that("Accessors work as expected on ``", {
#
# })

test_that("Accessors work as expected on `spaspm_data`", {
  TRUE
})

test_that("Accessors work as expected on `spaspm`", {

  # Valid
  expect_class({spm_name(spaspm_base)}, "character")

  expect_class({spm_unique_ID(spaspm_base)}, "character")
  expect_length({spm_unique_ID(spaspm_base)}, 1)

  expect_class({spm_coords_col(spaspm_base)}, "character")
  expect_length({spm_coords_col(spaspm_base)}, 2)

  expect_class({spm_boundaries(spaspm_base)}, "sf")

  expect_class({spm_base_dataset(spaspm_base)}, "spaspm_data")

  expect_class({spm_datasets(spaspm_base)}, "spaspm_data")
  expect_length({spm_datasets(spaspm_discrete)}, 1)

  # Not valid
  expect_message({spm_data(spaspm_base)},
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")
  expect_message({spm_discret_method(spaspm_base)},
                 "Model object 'Model test' is not a discrete model",)
  expect_message({spm_patches(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")
  expect_message({spm_points(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")
  expect_message({spm_mapped_datasets(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")

})

test_that("accessors work on `spaspm_discrete`", {

  # Valid
  expect_class({spm_name(spaspm_discrete)}, "character")

  expect_class({spm_unique_ID(spaspm_discrete)}, "character")
  expect_length({spm_unique_ID(spaspm_discrete)}, 1)

  expect_class({spm_coords_col(spaspm_discrete)}, "character")
  expect_length({spm_coords_col(spaspm_discrete)}, 2)

  expect_class({spm_boundaries(spaspm_discrete)}, "sf")

  expect_class({spm_base_dataset(spaspm_discrete)}, "spaspm_data")

  expect_class({spm_datasets(spaspm_discrete)}, "list")
  expect_length({spm_datasets(spaspm_discrete)}, 1)

  expect_class({spm_discret_method(spaspm_discrete)}, "discretization_method")

  expect_class({spm_patches(spaspm_discrete)},"sf")
  expect_class({spm_points(spaspm_discrete)}, "sf")

  expect_class({spm_mapped_datasets(spaspm_discrete)}, "list")
  expect_length({spm_mapped_datasets(spaspm_discrete)}, 0)

  # Not valid
  expect_message({spm_data(spaspm_discrete)},
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")
})

test_that("Accessors work as expected on `spaspm_discrete (MAPPED)`", {

  # Valid
  expect_class({spm_name(spaspm_discrete_mapped)}, "character")

  expect_class({spm_unique_ID(spaspm_discrete_mapped)}, "list")
  expect_length({spm_unique_ID(spaspm_discrete_mapped)}, 2)

  expect_class({spm_coords_col(spaspm_discrete_mapped)}, "list")
  expect_length({spm_coords_col(spaspm_discrete_mapped)}, 2)

  expect_class({spm_boundaries(spaspm_discrete_mapped)}, "sf")

  expect_class({spm_base_dataset(spaspm_discrete_mapped)}, "spaspm_data")

  expect_class({spm_datasets(spaspm_discrete_mapped)}, "list")
  expect_length({spm_datasets(spaspm_discrete_mapped)}, 2)

  expect_class({spm_discret_method(spaspm_discrete_mapped)}, "discretization_method")

  expect_class({spm_patches(spaspm_discrete_mapped)},"sf")
  expect_class({spm_points(spaspm_discrete_mapped)}, "sf")

  expect_class({spm_mapped_datasets(spaspm_discrete_mapped)}, "list")
  expect_length({spm_mapped_datasets(spaspm_discrete_mapped)}, 1)

  # Not valid
  expect_message({spm_data(spaspm_discrete_mapped)},
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")

})
