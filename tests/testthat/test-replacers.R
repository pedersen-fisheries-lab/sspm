# Test replacers for the sake of code coverage

test_that("Replacers work as expected", {

  expect_match({
    spm_name(sspm_base) <- "New Name"
    spm_name(sspm_base)
  }, "New Name")

})
