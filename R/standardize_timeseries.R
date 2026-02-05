#' Standardize timeseries across model outputs
#'
#' @param filepath Path to a file
#' @param pop_size_norm Population size used for normalization (e.g., pop_size_norm=1e5 would report values per 100K)
#' @param pop_size_abm (only used if input is an ABM file) Population size used for ABM simulation
#'
#' @returns A standardized data frame with normalized timeseries of outcomes
#' @export
standardize_timeseries <- function(filepath, pop_size_norm=1e5, pop_size_abm=2e6){
  ff <- parse_filename(filepath)
  pop.factor <- ifelse(ff$model=="abm", pop_size_abm, 41288599)/pop_size_norm

  df <- readr::read_csv(filepath, show_col_types = FALSE)

  # rename cols for abm
  if(ff$model=="abm"){
    df <- dplyr::rename(
      df,
      id_sim = IT, time = DAY, epi = OUTCOME,
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
      id_scenario, model, outcome, time, id_sim, age, value
    )
}
