requireNamespace("tidyr", quietly = TRUE)

names_ts <- c("id_scenario", "model", "outcome", "time", "id_sim", "age", "value")
names_ts_pt <- c("id_scenario", "model", "outcome", "time", "id_sim", "pt", "age", "value")

test_that("standardized timeseries have expected format for epac model", {
  df <- standardize_timeseries(testthat::test_path("fixtures", "epac_timeseries_flu1918_I0.csv"))

  # check names
  expect_equal(names(df), names_ts)

  # no missing outcome entries from lookup table
  expect_equal(df |> tidyr::drop_na() |> nrow(), nrow(df))
})

test_that("pt-disaggregated abm files aggregate across pt by default", {
  fp <- testthat::test_path("fixtures", "abm_timeseries_pt_sarscov2_I0.csv")

  df <- standardize_timeseries(fp)

  # aggregated output has the standard (no-pt) names
  expect_equal(names(df), names_ts)

  # no missing entries
  expect_equal(df |> tidyr::drop_na() |> nrow(), nrow(df))

  # "pt" token is stripped from id_scenario
  expect_equal(unique(df$id_scenario), "sarscov2_I0")
})

test_that("pt-disaggregated abm files can preserve pt with by_pt = TRUE", {
  fp <- testthat::test_path("fixtures", "abm_timeseries_pt_sarscov2_I0.csv")

  df_agg <- standardize_timeseries(fp)
  df_pt <- standardize_timeseries(fp, by_pt = TRUE)

  # disaggregated output includes a pt column
  expect_equal(names(df_pt), names_ts_pt)
  expect_setequal(unique(df_pt$pt), c("AB", "ON"))

  # summing across pt reproduces the aggregated output
  df_pt_summed <- df_pt |>
    dplyr::group_by(dplyr::across(-c(pt, value))) |>
    dplyr::summarize(value = sum(value), .groups = "drop")

  expect_equal(
    dplyr::arrange(df_pt_summed, outcome, time, id_sim, age),
    dplyr::arrange(df_agg, outcome, time, id_sim, age)
  )
})


test_that("standardized timeseries have expected format for abm model", {
  df <- standardize_timeseries(testthat::test_path("fixtures", "abm_timeseries_smallpox_I0.csv"))

  # check names
  expect_equal(names(df), names_ts)

  # no missing entries
  expect_equal(df |> tidyr::drop_na() |> nrow(), nrow(df))
})
