require(testthat)
context("gen_patch_insert")

test_that("the gen_patch_insert function works", {

  df1 <- data.frame(x = 1:10, y=rnorm(10))
  df2 <- data.frame(a = sample.int(10, 10), b = rnorm(10), c = letters[1:10])

  prefix <- "INSERT."
  result <- gen_patch_insert(df1, col1 = 2L, df2 = df2, col2 = 3L,
                             prefix = prefix)

  expect_true(is_patch(result))
  expect_identical(patch_type(result), expected = "insert")

  expect_identical(get_patch_params(result)[["insertion_point"]], 2L)

  expected <- data.frame(rep(NA, times = nrow(df1)))
  names(expected) <- paste0(prefix, "c")
  expect_identical(get_patch_params(result)[["data"]], expected)
})
