#' Standardize timeseries across model outputs
#'
#' @param filepath Path to a file
#' @param pop.factor_abm (only used if input is an ABM file) Population normalization factor for the ABM to rescale to outcomes per 100K individuals (since ABM can be run for different population sizes)
#'
#' @returns A standardized data frame with timeseries of outcome in terms of 100K individuals
#' @export
standardize_timeseries <- function(filepath, pop.factor_abm=20){
  ff <- parse_filename(filepath)
  pop.factor <- ifelse(ff$model=="abm", pop.factor_abm, 41288599/1e5)

  df <- readr::read_csv(filepath, show_col_types = FALSE)

  # rename cols for abm
  if(ff$model=="abm"){
    df <- dplyr::rename(
      df,
      id = IT, time = DAY, epi = OUTCOME,
      age = AGE_GRP, value = VALUE
    )
  }

  df |> dplyr::inner_join( # standardized outcome names
    lookup_outcome(),
    by = dplyr::join_by(epi == !!rlang::sym(ff$model))) |>
    dplyr::mutate(
      model = !!ff$model,
      id_scenario = !!ff$id_scenario
    ) |>
    dplyr::mutate(value = value/pop.factor) |>
    dplyr::select(
      id_scenario, model, outcome, time, id, age, value
    )
}
