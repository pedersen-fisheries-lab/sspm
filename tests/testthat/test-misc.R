# Testing miscellaneous functions

test_that("Methods are correctly returned", {
  expect_class({spm_methods()}, "character")
  expect_length({spm_methods()}, 1)
})

test_that("Functons for methods are correctly dispatched", {
  expect_class({spaspm:::dispatch_method("tesselate_voronoi")}, "function")
})
