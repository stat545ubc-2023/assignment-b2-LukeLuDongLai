test_that("Output matches direct call to dplyr", {
  small_tbl <- tribble(~group, ~var1, ~var2,
                       "A", 1, NA,
                       "A", 2, "x",
                       "B", NA, "y",
                       "C", 3, "z")

  expect_equal( small_tbl |> group_by(group) |>
                  summarize(across(everything(), ~sum(is.na(.x))),
                            .groups = "drop"),
                count_all_missing_by_group(small_tbl, group)
  )

  expect_equal( small_tbl |> group_by(group) |>
                  summarize(across(everything(), ~sum(is.na(.x))),
                            .groups = NULL),
                count_all_missing_by_group(small_tbl, group, NULL)
  )
})

test_that("Handle no missing values correctly", {
  no_na_tbl <- tibble(group = c("A", "A", "B", "B"), var1 = 1:4, var2 = 4:1)
  expect_equal(
    count_all_missing_by_group(no_na_tbl, group),
    tibble(group = c("A", "B"), var1 = c(0, 0), var2 = c(0, 0))
  )
})

test_that("Handle columns with all missing values correctly", {
  all_na_col_tbl <- tibble(group = c("A", "A", "B", "B"), var1 = c(1, 2, NA, 4), var2 = NA)
  expected_result <- tibble(group = c("A", "B"), var1 = c(0, 1), var2 = c(2, 2))
  expect_equal(
    count_all_missing_by_group(all_na_col_tbl, group),
    expected_result
  )
})

test_that("Checking error handling for .groups input", {
  expect_error(
    count_all_missing_by_group(airquality, Month, "kep")
  )
  expect_no_error(
    count_all_missing_by_group(airquality, Month, NULL)
  )
})

test_that("Handle non-existing group column name", {
  expect_error(count_all_missing_by_group(airquality, non_existing_column))
})

test_that("Handle non-data.frame or non-tibble input", {
  expect_error(count_all_missing_by_group(as.matrix(airquality), Month))
})
