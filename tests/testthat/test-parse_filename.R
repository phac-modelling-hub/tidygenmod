requireNamespace("glue", quietly = TRUE)

test_that("parse filename into list with correct format", {
  model <- "epac"
  filetype <- "timeseries"
  id_scenario <- "sarscov2"

  # check basic filename (disease suffix only)
  expect_identical(
  parse_filename(glue::glue("{model}_{filetype}_{id_scenario}.csv")),
   list(
     model = model,
     filetype = filetype,
     id_scenario = id_scenario
   )
  )

  # check with intervention id
  id_scenario <- glue::glue("{id_scenario}_I0")
  expect_identical(
    parse_filename(glue::glue("{model}_{filetype}_{id_scenario}.csv")),
    list(
      model = model,
      filetype = filetype,
      id_scenario = id_scenario
    )
  )

  # check with directory prefix
  expect_identical(
    parse_filename(file.path("data", glue::glue("{model}_{filetype}_{id_scenario}.csv"))),
    list(
      model = model,
      filetype = filetype,
      id_scenario = id_scenario
    )
  )
})
