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

  expect_class({
    spm_datasets(sspm_discrete) <- list()
    spm_datasets(sspm_discrete)
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

  expect_names({
    formula_vars(sspm_formula) <- list(a=1, b=2)
    names(formula_vars(sspm_formula))
  }, identical.to = c("a", "b"))

  # Data

  expect_data_frame({
    spm_data(sspm_dataset) <- mtcars
    spm_data(sspm_dataset)
  })

  expect_match({
    spm_name(sspm_dataset) <- "NewDatasetName_2"
    spm_name(sspm_dataset)
  }, "NewDatasetName_2")

  expect_match({
    spm_coords_col(sspm_dataset) <- c("one_2", "two_2")
    spm_coords_col(sspm_dataset)[1]
  }, c("one_2"))

  expect_match({
    spm_coords_col(sspm_dataset) <- c("one_2", "two_2")
    spm_coords_col(sspm_dataset)[2]
  }, c("two_2"))

  expect_match({
    spm_unique_ID(sspm_dataset) <- "New_ID_2"
    spm_unique_ID(sspm_dataset)
  }, "New_ID_2")

  expect_match({
    spm_time_column(sspm_dataset) <- "new_time_column_2"
    spm_time_column(sspm_dataset)
  }, "new_time_column_2")

})
