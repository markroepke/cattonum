context("mean encoding")

expected_df_both <- data.frame(
  y = y,
  x1 = c(49 / 3, 5, NA, 5, 49 / 3, 49 / 3),
  x2 = c(39 / 4, 39 / 4, 39 / 4, 12, 12, 39 / 4)
)

expected_tbl_both <- dplyr::as_tibble(expected_df_both)

expected_x1_df_fact <- data.frame(y,
  x1 = c(49 / 3, 5, NA, 5, 49 / 3, 49 / 3),
  x2
)

expected_x1_df_char <- data.frame(y,
  x1 = c(49 / 3, 5, NA, 5, 49 / 3, 49 / 3),
  x2,
  stringsAsFactors = FALSE
)

expected_x1_tbl_char <- dplyr::tibble(y, x1 = c(49 / 3, 5, NA, 5, 49 / 3, 49 / 3), x2)

expected_x1_tbl_fact <- dplyr::tibble(y,
  x1 = c(49 / 3, 5, NA, 5, 49 / 3, 49 / 3),
  x2 = factor(x2)
)

encoded_test <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, 49 / 3, 5, 5),
  x2 = c(12, NA, NA, 39 / 4, 39 / 4)
)

test_that("catto_mean: multiple data.frame training columns.", {
  both_encoded <- check_x1_x2_resp(catto_mean, "data.frame", .resp = resp_name)
  for (m in both_encoded) expect_equal(m, expected_df_both)
})

test_that("catto_mean: multiple tibble training columns.", {
  both_encoded <- check_x1_x2_resp(catto_mean, "tibble", .resp = resp_name)
  for (m in both_encoded) expect_equal(m, expected_tbl_both)
})

test_that("catto_mean: one data.frame training column.", {
  one_encoded <- check_x1_resp(catto_mean, "data.frame", .resp = resp_name)
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_equal(one_encoded[[i]], expected_x1_df_fact)
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_equal(one_encoded[[i]], expected_x1_df_char)
  }
})

test_that("catto_mean: one tibble training column.", {
  one_encoded <- check_x1_resp(catto_mean, "tibble", .resp = resp_name)
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_equal(one_encoded[[i]], expected_x1_tbl_fact)
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_equal(one_encoded[[i]], expected_x1_tbl_char)
  }
})

test_that("catto_mean correctly encodes test data.", {
  expect_equal(
    catto_mean(df_fact, test = test_df),
    list(train = expected_df_both, test = encoded_test)
  )
})
