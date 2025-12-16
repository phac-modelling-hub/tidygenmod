test_that("parse filename into list with correct format", {
  model <- "epac"
  filetype <- "timeseries"
  id_scenario <- "sarscov2_I0"
  ff <- paste0(paste(model, filetype, id_scenario, sep = "_"), ".csv")

  expect_identical(
  parse_filename(ff),
   list(
     model = model,
     filetype = filetype,
     id_scenario = id_scenario
   )
  )

  expect_identical(
    parse_filename(file.path("data", ff)),
    list(
      model = model,
      filetype = filetype,
      id_scenario = id_scenario
    )
  )
})
