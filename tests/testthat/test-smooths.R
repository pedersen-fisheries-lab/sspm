# Test the behavior of the smooth formula functions

test_that("Low - level matrix functions work as expected", {

  # Time
  time_levels <- c(1991, 1992, 1993, 1994)
  pen_mat_time <- sspm:::ICAR_time(c(1991, 1992, 1993, 1994))

  pen_mat_time_compare <- matrix(c(1, -1, 0, 0,
                                   -1, 2, -1, 0,
                                   0, -1, 2, -1,
                                   0, 0, -1, 1), 4, 4)
  dimnames(pen_mat_time_compare) <- list(time_levels, time_levels)

  expect_identical(pen_mat_time, pen_mat_time_compare)
  expect_true(sum(rowSums(pen_mat_time) != 0) == 0)
  expect_true(sum(colSums(pen_mat_time) != 0) == 0)

  # Space
  pen_mat_space <- sspm:::ICAR_space(borealis_patches[1:5, ], "patch_id")

  pen_mat_space_compare <- matrix(c(2, -1, -1, 0, 0,
                                    -1, 2, -1, 0, 0,
                                    -1, -1, 3, 0, -1,
                                    0, 0, 0, 0, 0,
                                    0, 0, -1, 0, 1), 5, 5)
  names_dim <- c("P1", "P2", "P3", "P4", "P5")
  dimnames(pen_mat_space_compare) <- list(names_dim, names_dim)

  expect_identical(pen_mat_space, pen_mat_space_compare)
  expect_true(sum(rowSums(pen_mat_space) != 0) == 0)
  expect_true(sum(colSums(pen_mat_space) != 0) == 0)

})

test_that("Smooths are assembles correctly", {

  expect_identical(sspm:::assemble_smooth("s", list(a = "ns", b = 3, c = list(d = "hn", e = 4))),
                   "s(a = \"ns\", b = 3, c = list(d = \"hn\", e = 4))")

})

test_that("Main smooth functions work as expected", {
  skip("TODO")

  # Failures => This avctually test ICAR
  expect_error(smooth_time(dataset = "NotValid", sspm_object = sspm_discrete_mapped),
               "Argument 'dataset' must be one of: Biomass, Predator")

  # Successes
  res_time <- smooth_time(dataset = "Biomass", sspm_object = sspm_discrete_mapped)
  expect_equal(res_time$smooth, "s(year_f, k = 24L, bs = \"re\", xt = list(penalty = pen_mat_time))")
  expect_names(names(res_time$vars), identical.to = c("pen_mat_time"))
  expect_equal(dim(res_time$vars$pen_mat_time), c(24, 24))

  res_space <- smooth_space(dataset = "Biomass", sspm_object = sspm_discrete_mapped)
  expect_equal(res_space$smooth, "s(patch_id, k = 30, bs = \"mrf\", xt = list(penalty = pen_mat_space))")
  expect_names(names(res_space$vars), identical.to = c("pen_mat_space"))
  expect_equal(dim(res_space$vars$pen_mat_space), c(69, 69))

  res_space_time <- smooth_space_time(dataset = "Biomass", sspm_object = sspm_discrete_mapped)
  expect_equal(res_space_time$smooth,
               paste0("ti(year_f, patch_id, k = c(24, 30), ",
                      "bs = c(\"re\", \"mrf\"), xt = list(year_f = list(penalty = pen_mat_time), ",
                      "patch_id = list(penalty = pen_mat_space)))"))
  expect_names(names(res_space_time$vars), identical.to = c("pen_mat_time", "pen_mat_space"))
  expect_equal(dim(res_space_time$vars$pen_mat_time), c(24, 24))
  expect_equal(dim(res_space_time$vars$pen_mat_space), c(69, 69))

})

test_that("ICAR function works as expected", {
  skip("TODO")

  # Just basic output
  res_ICAR <- sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "time",
                          k = NULL, xt = NULL, bs = NULL)
  expect_equal(res_ICAR$args[[1]], substitute(year_f))
  expect_equal(res_ICAR$args$k, 24)
  expect_equal(res_ICAR$args$bs, "re")
  expect_equal(res_ICAR$args$xt, alist(penalty = pen_mat_time))

  res_ICAR <- sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "space",
                          k = NULL, xt = NULL, bs = NULL)
  expect_equal(res_ICAR$args[[1]], substitute(patch_id))
  expect_equal(res_ICAR$args$k, 30)
  expect_equal(res_ICAR$args$bs, "mrf")
  expect_equal(res_ICAR$args$xt, alist(penalty = pen_mat_space))

  res_ICAR <- sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "space_time",
                          k = NULL, xt = NULL, bs = NULL)
  expect_equal(res_ICAR$args[[1]], substitute(year_f))
  expect_equal(res_ICAR$args[[2]], substitute(patch_id))
  expect_equal(res_ICAR$args$k, c(24, 30))
  expect_equal(res_ICAR$args$bs, c("re", "mrf"))
  expect_equal(res_ICAR$args$xt, list(year_f = alist(penalty = pen_mat_time),
                                      patch_id = alist(penalty = pen_mat_space)))

  # Now test xt cases
  # Cannot handle cases where other arguments are passed as list in xt
  test_mat <- matrix(0, 5, 5)

  expect_identical({
    sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "space",
                k = NULL, xt = list(penalty = test_mat), bs = NULL)$vars$pen_mat_space$penalty
  }, test_mat)

  expect_identical({
    sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "time",
                k = NULL, xt = list(penalty = test_mat), bs = NULL)$vars$pen_mat_time$penalty
  }, test_mat)

  expect_identical({
    sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "space_time",
                k = NULL, xt = list(patch_id = list(penalty = matrix(0, 5, 5))), bs = NULL)$vars$pen_mat_space
  }, test_mat)

  expect_identical({
    sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "space_time",
                k = NULL, xt = list(year_f = list(penalty = matrix(0, 5, 5))), bs = NULL)$vars$pen_mat_time
  }, test_mat)

  # Failures
  expect_error({
    sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "space_time",
                k = NULL, xt = list(year_f = list(penalty = mtcars)), bs = NULL)
  }, "Must be of type 'matrix', not 'data.frame'")

  expect_error({
    sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "space_time",
                k = NULL, xt = list(list(penalty = test_mat)), bs = NULL)
  }, "failed: Must have names.")

  expect_error({
    sspm:::ICAR(sspm_discrete_mapped, "Biomass", dimension = "space_time",
                k = NULL, xt = list(penalty = test_mat), bs = NULL)
  }, "failed: Must be of type 'list', not 'matrix'.")
})
