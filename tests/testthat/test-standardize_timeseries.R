requireNamespace("tidyr", quietly = TRUE)

names_ts <- c("id_scenario", "model", "outcome", "time", "id", "age", "value")

test_that("standardized timeseries have expected format for epac model", {
  df <- standardize_timeseries(testthat::test_path("fixtures", "epac_timeseries_flu1918_I0.csv"))

  # check names
  expect_equal(names(df), names_ts)

  # no missing outcome entries from lookup table
  expect_equal(df |> tidyr::drop_na() |> nrow(), nrow(df))
})


test_that("standardized timeseries have expected format for abm model", {
  df <- standardize_timeseries(testthat::test_path("fixtures", "abm_timeseries_smallpox_I0.csv"))

  # check names
  expect_equal(names(df), names_ts)

  # no missing entries
  expect_equal(df |> tidyr::drop_na() |> nrow(), nrow(df))
})
