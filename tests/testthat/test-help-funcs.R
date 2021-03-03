# Test helper functions
#
# test_that("", {
#
# })

# TODO add tests for check_model_family, rqresiduals

test_that("Methods choices are retrieved", {

  choices <- spm_methods()
  expect_character(choices)
  expect_names(choices, identical.to = "tesselate_voronoi" )

  smooth_choices <- spm_smooth_methods()
  expect_character(smooth_choices)
  expect_names(smooth_choices, identical.to = "ICAR" )

})

test_that("Methods are dispatched correctly", {

  expect_function(dispatch_method("tesselate_voronoi"))
  expect_message(dispatch_method("method_not_supported"),
                 "Method 'method_not_supported' is not part of the supported methods.")

  expect_function(dispatch_smooth("ICAR"))
  expect_message(dispatch_smooth("method_not_supported"),
                 "Smoothing method 'method_not_supported' is not part of the supported methods.")

})

test_that("Function length_to_weigth works correctly", {

  expect_equal(length_to_weigth(10, "male"), 0.6331151)
  expect_equal(length_to_weigth(10, "female"), 0.8882952)

})

test_that("Warnings/messages can be suppressed", {

  expect_failure(expect_warning(suppressAll(warning("This is a warning"))))
  expect_failure(expect_message(suppressAll(message("This is a message"))))

})

test_that("Calls are modified correctly", {

  base_col <- str2lang("s()")
  modified_call <- modify_call(base_col, list(k=1, bs="mrf"))
  modified_call_str <- deparse(modified_call)

  expect_class(modified_call, "call")
  expect_match(modified_call_str, "s(k = 1, bs = \"mrf\")", fixed = TRUE)

})

test_that("Families are checked correctly", {

  gam_obj <- mgcv::gam(mpg ~ s(drat), data = mtcars, family = mgcv::tw)
  fam <- gam_obj$family$family
  spaspm:::check_model_family(fam)

  fam <- "crazy_family"
  expect_error(spaspm:::check_model_family(fam))

})

test_that("Residuals are correctly retrieved", {

  # Tweedie case
  gam_obj <- mgcv::gam(mpg ~ s(drat), data = mtcars, family = mgcv::tw)
  res <- spaspm:::rqresiduals(gam_obj)

  expect_length(res, 32)
  expect_equal(min(res), -1.860841, tolerance=1e-07)
  expect_equal(max(res), 1.872867, tolerance=1e-07)
  expect_equal(median(res), -0.0278781, tolerance=1e-06)

  # Neg bin case
  gam_obj <- mgcv::gam(mpg ~ s(drat), data = mtcars, family = mgcv::nb)
  set.seed(77);res <- spaspm:::rqresiduals(gam_obj)

  expect_length(res, 32)
  expect_equal(min(res), -1.870483, tolerance=1e-07)
  expect_equal(max(res), 1.899658, tolerance=1e-07)
  expect_equal(median(res), -0.1017879, tolerance=1e-06)

})
