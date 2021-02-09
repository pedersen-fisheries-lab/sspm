# Test accessors
test_that("accessors work on spaspm", {

  expect_class({spm_name(spaspm_base)}, "character")
  expect_class({spm_base_dataset(spaspm_base)}, "spaspm_data")
  expect_class({spm_boundaries(spaspm_base)}, "sf")

  expect_message({spm_data(spaspm_base)},
                 "Use `spm_datasets` or `spm_base_dataset` to access the datasets of a spaspm object")
  expect_message({spm_discret_method(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")
  expect_message({spm_patches(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")
  expect_message({spm_points(spaspm_base)},
                 "Model object 'Model test' is not a discrete model")
})

test_that("accessors work on spaspm_discrete", {

  expect_class({spm_name(spaspm_discrete)}, "character")
  expect_class({spm_base_dataset(spaspm_discrete)}, "list")
  expect_class({spm_boundaries(spaspm_discrete)}, "sf")

  expect_class({spm_discret_method(spaspm_discrete)}, "discretization_method")
  expect_class({spm_patches(spaspm_discrete)}, "sf")
  expect_class({spm_points(spaspm_discrete)}, "sf")
})


