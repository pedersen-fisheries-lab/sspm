# Test the show methods

test_that("Shpw methods print out fine", {

  expect_output(show(sspm_base))
  expect_output(show(sspm_data))
  expect_output(show(sspm_formula))
  expect_output(show(discret_method))
  expect_output(show(sspm_discrete))

})
