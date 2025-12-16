requireNamespace("tidyr", quietly = TRUE)

test_that("age-aggregation works", {
  time <- 1:2
  age <- c("0-64", "65+")
  value_t1 <- c(2,7)
  value_t2 <- c(4,3)

  df <- tidyr::expand_grid(
    time = time,
    age = age
  ) |>
    dplyr::mutate(value = c(value_t1, value_t2))

  expect_equal(
    aggregate_across_ages(df),
    tibble::tibble(
      time = time,
      value = c(sum(value_t1), sum(value_t2))
    )
  )
})
