require(testthat)
context("ddiff")

test_that("the ddiff function works", {

  generate_normal_df <- function(n) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 4)
    v3 <- rnorm(n, mean = 2)
    v4 <- rnorm(n, sd = 2, mean = 4)
    v5 <- rnorm(n, sd = 4, mean = 8)
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5)
  }

  set.seed(22)
  df1 <- generate_normal_df(100)
  df2 <- generate_normal_df(101)

  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99, permute_penalty = 0.1)
  expect_true(is_patch(result, allow_composed = FALSE))
  expect_equal(patch_type(result), "identity")

  perm <- as.integer(c(2, 3, 1, 4, 5))

  # Note that in this case, due to the similarity between v1 and the shifted v3,
  # we must take a large transform patch penalty and a small permute penalty.
  # Q: why do we not see this in the alternative (original) algorithm?
  result <- ddiff(df1, df2 = df2[perm], patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.7, break_penalty = 0.99, permute_penalty = 0.1)
  expect_equal(patch_type(result), "perm")
  expect_identical(get_patch_params(result)[["perm"]], expected = perm)

  ## Test with an affine transformation.
  set.seed(22)
  df1 <- generate_normal_df(100)
  df2 <- generate_normal_df(101)

  df2[[1]] <- 32 + (9/5) * df2[[1]]
  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99,
                  permute_penalty = 0.1, as.list = TRUE)

  # Affine transform is correctly identified, with (approximately) correct
  # parameters.
  expect_equal(length(result), expected = 2)
  expect_equal(patch_type(result[[1]]), expected = "scale")
  expect_equal(get_patch_params(result[[1]])[["cols"]], expected = 1)
  expect_equal(get_patch_params(result[[1]])[["scale_factor"]], expected = 2,
               tolerance = 0.1)
  expect_equal(patch_type(result[[2]]), "shift")
  expect_equal(get_patch_params(result[[2]])[["cols"]], expected = 1)
  expect_equal(get_patch_params(result[[2]])[["shift"]], expected = 32,
               tolerance = 0.01)

  ## Repeat the affine test with more difficult parameters...
  set.seed(22)
  df1 <- generate_normal_df(100)
  df2 <- generate_normal_df(101)

  df2[[1]] <- 2 + (7/5) * df2[[1]]
  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99,
                  permute_penalty = 0.1, as.list = TRUE)

  # ...still works.
  expect_equal(length(result), expected = 2)
  expect_equal(patch_type(result[[1]]), expected = "scale")
  expect_equal(get_patch_params(result[[1]])[["cols"]], expected = 1)
  expect_equal(get_patch_params(result[[1]])[["scale_factor"]], expected = 2,
               tolerance = 0.3)
  expect_equal(patch_type(result[[2]]), "shift")
  expect_equal(get_patch_params(result[[2]])[["cols"]], expected = 1)
  expect_equal(get_patch_params(result[[2]])[["shift"]], expected = 2,
               tolerance = 0.02)

  ## Repeat again with even more difficult parameters...
  set.seed(22)
  df1 <- generate_normal_df(100)
  df2 <- generate_normal_df(101)

  df2[[1]] <- 2 + (6/5) * df2[[1]]
  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99,
                  permute_penalty = 0.1, as.list = TRUE)

  # ...still works.
  expect_equal(length(result), expected = 2)
  expect_equal(patch_type(result[[1]]), expected = "scale")
  expect_equal(patch_type(result[[2]]), expected = "shift")

  ## Test with differing numbers of columns.
  set.seed(22)
  df1 <- generate_normal_df(100)
  df2 <- generate_normal_df(101)

  result <- ddiff(df1, df2 = df2[1:3], patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99, permute_penalty = 0.1)
  expect_true(is_patch(result, allow_composed = TRUE))
  expect_equal(length(decompose_patch(result)), expected = 2)
  expect_equal(patch_type(decompose_patch(result)[[1]]), "delete")
  expect_identical(get_patch_params(decompose_patch(result)[[1]])[["cols"]],
                   expected = 5L)
  expect_equal(patch_type(decompose_patch(result)[[2]]), "delete")
  expect_identical(get_patch_params(decompose_patch(result)[[2]])[["cols"]],
                   expected = 4L)
  expect_identical(names(result(df1)), expected = names(df2[1:3]))

  ## Test with a permutation only (but with differing numbers of columns)
  perm <- as.integer(c(2, 5, 1, 4, 3))

  result <- ddiff(df1, df2 = df2[perm][1:3], patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99, permute_penalty = 0.1)
  expect_true(is_patch(result, allow_composed = TRUE))

  expect_identical(names(result(df1)), expected = names(df2[perm][1:3]))

  # Note: very high patch_penalty required here for correct identification of the
  # permutation.
  insert_col_name <- "INSERT"
  result <- ddiff(df1[1:3], df2 = df2[perm], patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.8, break_penalty = 0.99, permute_penalty = 0.1,
                  insert_col_name = insert_col_name)
  expect_true(is_patch(result, allow_composed = TRUE))

  expected <- names(df2[perm])
  expected[which(expected %in% c("v4", "v5"))] <- insert_col_name
  expect_identical(names(result(df1[1:3])), expected = expected)

  #### Test with mixed data types.

  generate_mixed_df <- function(n = 100) {
    v1 <- rnorm(n)
    v2 <- rnorm(n, sd = 4, mean = 1)
    v3 <- rexp(n)
    v4 <- sample.int(10, size = n, replace = TRUE)
    v5 <- sample(c("M", "F"), size = n, replace = TRUE, prob = c(1/4, 3/4))
    data.frame("v1" = v1, "v2" = v2, "v3" = v3, "v4" = v4, "v5" = v5,
               stringsAsFactors = FALSE)
  }

  set.seed(22)
  df1 <- generate_mixed_df(100)
  df2 <- generate_mixed_df(101)

  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99, permute_penalty = 0.1)

  expect_true(is_patch(result, allow_composed = FALSE))
  expect_equal(patch_type(result), "identity")

  # Reducing the penalty associated with a transformation patch changes the
  # result: the cost of an affine patch becomes less than improvement in mismatch.
  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.4, break_penalty = 0.99, permute_penalty = 0.1)

  expect_true(is_patch(result, allow_composed = TRUE))
  expect_false(is_patch(result, allow_composed = FALSE))

  ## Test with a shift.
  set.seed(22)
  df1 <- generate_mixed_df(100)
  df2 <- generate_mixed_df(100)
  df2[[1]] <- 4 + df2[[1]]

  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99,
                  permute_penalty = 0.1, as.list = TRUE)

  # Test for the expected result.
  expect_equal(length(result), 2)
  expect_equal(patch_type(result[[1]]), "scale")
  expect_equal(patch_type(result[[2]]), "shift")

  ## Test with the numeric columns permuted in df1 and a shift.
  set.seed(22)
  df1 <- generate_mixed_df(100)
  df2 <- generate_mixed_df(101)
  df2[[1]] <- 4 + df2[[1]]
  perm <- as.integer(c(2, 3, 1, 4, 5))
  df2 <- df2[perm]

  # Note: very high break penalty required for correct identification here.
  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.9999,
                  permute_penalty = 0.1, as.list = TRUE)

  # Test for the expected result.
  expect_equal(length(result), 3)
  expect_equal(patch_type(result[[1]]), "scale")
  expect_equal(patch_type(result[[2]]), "shift")
  expect_equal(patch_type(result[[3]]), "perm")
  expect_equal(get_patch_params(result[[3]])[["perm"]], perm)

  ## Test with recodings of categorical data.
  set.seed(22)
  df1 <- generate_mixed_df(100)
  df2 <- generate_mixed_df(101)

  df2[[5]] <- ifelse(df2[[5]] == "M", yes = 1L, no = 0L)

  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99, permute_penalty = 0.1)

  expect_true(is_patch(result, allow_composed = FALSE))
  expect_identical(patch_type(result, short = TRUE), expected = "recode")
  expect_identical(get_patch_params(result)[["encoding"]],
                   expected = c("F" = 0L, "M" = 1L))

  perm <- as.integer(c(2, 3, 1, 4, 5))
  df2 <- df2[perm]

  result <- ddiff(df1, df2 = df2, patch_generators = list(gen_patch_transform),
                  patch_penalties = 0.6, break_penalty = 0.99,
                  permute_penalty = 0.1, as.list = TRUE)

  expect_equal(length(result), expected = 2)
  expect_identical(patch_type(result[[1]], short = TRUE),
                   expected = "recode")
  expect_identical(get_patch_params(result[[1]])[["encoding"]],
                   expected = c("F" = 0L, "M" = 1L))

  expect_identical(patch_type(result[[2]], short = TRUE),
                   expected = "perm")
  expect_identical(get_patch_params(result[[2]])[["perm"]],
                   expected = perm)



  # TODO: test with data frames containing both factors and integer data (by
  # setting stringsAsFactors = TRUE above).

  # IMP TODO: test with (significantly) differing numbers of rows in df1 & df2.

})
