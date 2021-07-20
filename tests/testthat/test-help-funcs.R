# Test helper functions
#
# test_that("", {
#
# })

test_that("Methods choices are retrieved", {

  choices <- spm_methods()
  expect_character(choices)
  expect_names(choices, identical.to = c("tesselate_voronoi", "triangulate_delaunay"))

  smooth_choices <- spm_smooth_methods()
  expect_character(smooth_choices)
  expect_names(smooth_choices, identical.to = c("ICAR", "LINPRED"))

})

test_that("Aggregation choices are retrieved", {

  choices <- spm_aggregation_choices()
  expect_equal(choices, c('space', 'time', 'spacetime'))

})

test_that("Methods are dispatched correctly", {

  expect_equal(dispatch_method("tesselate_voronoi"), tesselate_voronoi)
  expect_equal(dispatch_method("triangulate_delaunay"), triangulate_delaunay)
  expect_message(dispatch_method("method_not_supported"),
                 "Method 'method_not_supported' is not part of the supported methods.")

  expect_equal(dispatch_smooth("ICAR"), ICAR)
  expect_equal(dispatch_smooth("LINPRED"), LINPRED)
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
  modified_call <- modify_call(base_col, list(k = 1, bs = "mrf"))
  modified_call_str <- deparse(modified_call)

  expect_class(modified_call, "call")
  expect_match(modified_call_str, "s(k = 1, bs = \"mrf\")", fixed = TRUE)

})

test_that("Families are checked correctly", {

  gam_obj <- mgcv::gam(mpg ~ s(drat), data = mtcars, family = mgcv::tw)
  fam <- gam_obj$family$family
  sspm:::check_model_family(fam)

  fam <- "crazy_family"
  expect_error(sspm:::check_model_family(fam))

})

test_that("Residuals are correctly retrieved", {

  # Tweedie case
  gam_obj <- mgcv::gam(mpg ~ s(drat), data = mtcars, family = mgcv::tw)
  res <- sspm:::rqresiduals(gam_obj)

  expect_length(res, 32)
  expect_equal(min(res), -1.860841, tolerance = 1e-07)
  expect_equal(max(res), 1.872867, tolerance = 1e-07)
  expect_equal(median(res), -0.0278781, tolerance = 1e-06)

  # Neg bin case
  gam_obj <- mgcv::gam(mpg ~ s(drat), data = mtcars, family = mgcv::nb)
  set.seed(77);res <- sspm:::rqresiduals(gam_obj)

  expect_length(res, 32)
  expect_equal(min(res), -1.870483, tolerance = 1e-07)
  expect_equal(max(res), 1.899658, tolerance = 1e-07)
  expect_equal(median(res), -0.1017879, tolerance = 1e-06)

})

# Testing miscellaneous functions

test_that("Methods are correctly returned", {
  expect_class({spm_methods()}, "character")
  expect_length({spm_methods()}, 2)
})

test_that("Functons for methods are correctly dispatched", {
  expect_class({sspm:::dispatch_method("tesselate_voronoi")}, "function")
})

test_that("Not discrete message works", {
  expect_message(message_not_discrete(biomass_dataset),
                 "Model object 'Biomass' is not a discrete model")
})

test_that("Multilag works", {
  expect_equal(multilag(c(1:5), 2),
               data.frame(lag1 = c(NA, 1, 2, 3, 4),
                          lag2 = c(NA, NA, 1, 2, 3)))
})
