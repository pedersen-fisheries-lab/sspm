# Test replacers for the sake of code coverage

test_that("Replacers work as expected", {

  # Non discrete

  expect_match({
    spm_name(sspm_base) <- "New_Name"
    spm_name(sspm_base)
  }, "New_Name")

  expect_match({
    spm_name(discret_method) <- "New_Name"
    spm_name(discret_method)
  }, "New_Name")

  expect_match({
    spm_coords_col(sspm_base) <- c("one", "two")
    spm_coords_col(sspm_base)[1]
  }, c("one"))

  expect_match({
    spm_coords_col(sspm_base) <- c("one", "two")
    spm_coords_col(sspm_base)[2]
  }, c("two"))

  expect_match({
    spm_unique_ID(sspm_base) <- "New_ID"
    spm_unique_ID(sspm_base)
  }, "New_ID")

  expect_match({
    spm_time_col(sspm_base) <- "new_time_col"
    spm_time_col(sspm_base)
  }, "new_time_col")

  expect_class({
    spm_base_dataset(sspm_base) <- sspm_data
    spm_base_dataset(sspm_base)
  }, "sspm_data")

  expect_class({
    spm_boundaries(sspm_base) <- sfa_boundaries
    spm_boundaries(sspm_base)
  }, "sf")

  expect_class({
    spm_boundaries(sspm_discrete) <- sfa_boundaries
    spm_boundaries(sspm_discrete)
  }, "sf")

  # Discrete

  expect_message({
    spm_discret_method(sspm_base) <- discret_method
  }, "Model object 'New_Name' is not a discrete model")

  expect_class({
    spm_discret_method(sspm_discrete) <- discret_method
    spm_discret_method(sspm_discrete)
  }, "discretization_method")

  expect_message({
    spm_patches(sspm_base) <- borealis_patches
  }, "Model object 'New_Name' is not a discrete model")

  expect_class({
    spm_patches(sspm_discrete) <- borealis_patches
    spm_patches(sspm_discrete)
  }, "sf")

  expect_message({
    spm_points(sspm_base) <- borealis_points
  }, "Model object 'New_Name' is not a discrete model")

  expect_class({
    spm_points(sspm_discrete) <- borealis_points
    spm_points(sspm_discrete)
  }, "sf")

  expect_message({
    spm_mapped_datasets(sspm_base) <- borealis_points
  }, "Model object 'New_Name' is not a discrete model")

  expect_class({
    spm_mapped_datasets(sspm_discrete) <- list()
    spm_mapped_datasets(sspm_discrete)
  }, "list")

  expect_message({
    spm_mapped_formulas(sspm_base) <- borealis_points
  }, "Model object 'New_Name' is not a discrete model")

  expect_class({
    spm_mapped_formulas(sspm_discrete) <- list()
    spm_mapped_formulas(sspm_discrete)
  }, "list")

  # Method

  expect_function({
    method_func(discret_method) <- rnorm
    method_func(discret_method)
  })

  # Formula
  expect_match({
    sspm:::format_formula(raw_formula(sspm_formula) <- as.formula(a ~ b))
  }, "a ~ b")

  expect_match({
    sspm:::format_formula(translated_formula(sspm_formula) <- as.formula(c ~ d))
  }, "c ~ d")

  expect_match({
    dataset(sspm_formula) <- "NewDatasetName"
  }, "NewDatasetName")

  expect_names({
    formula_vars(sspm_formula) <- list(a=1, b=2)
    names(formula_vars(sspm_formula))
  }, identical.to = c("a", "b"))

})
