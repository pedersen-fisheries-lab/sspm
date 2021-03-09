# Simple tests for plotting functions

test_that("Plotting works as expected", {

  expect_message(plot(sspm_base),
                 "This model is not discretized and cannot be plotted")

})
