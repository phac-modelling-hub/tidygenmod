#' Standardize timeseries across model outputs
#'
#' @param filepath Path to a file
#' @param pop_size_norm Population size used for normalization (e.g., pop_size_norm=1e5 would report values per 100K)
#' @param pop_size_abm (only used if input is an ABM file) Population size used for ABM simulation
#' @param by_pt (logical) If the input file is disaggregated by province/territory
#'   (i.e., it has a `PT` column), should the standardized output preserve that
#'   disaggregation? When `FALSE` (the default), values are summed across
#'   provinces/territories to reproduce the aggregated output. When `TRUE`, a
#'   `pt` column is added to the output. Has no effect for files without a `PT`
#'   column.
#'
#' @returns A standardized data frame with normalized timeseries of outcomes. When
#'   `by_pt = TRUE` and the input is disaggregated by province/territory, the
#'   output includes an additional `pt` column.
#' @export
standardize_timeseries <- function(filepath, pop_size_norm=1e5, pop_size_abm=2e6, by_pt=FALSE){
  ff <- parse_filename(filepath)
  pop.factor <- ifelse(ff$model=="abm", pop_size_abm, 41288599)/pop_size_norm

  df <- readr::read_csv(filepath, show_col_types = FALSE)

  # rename cols for abm
  if(ff$model=="abm"){
    df <- dplyr::rename(df, id_sim = IT, time = DAY, age = AGE_GRP)
    if("OUTCOME" %in% names(df)) df <- dplyr::rename(df, epi = OUTCOME)
    if("VALUE" %in% names(df)) df <- dplyr::rename(df, value = VALUE)
  }

  # normalize simulation id column to id_sim (epac files use `id`)
  if("id" %in% names(df)) df <- dplyr::rename(df, id_sim = id)

  # detect province/territory disaggregation
  has_pt <- "PT" %in% names(df)
  if(has_pt) df <- dplyr::rename(df, pt = PT)

  # pivot wide -> long when outcomes are stored as columns
  # (files already in long format have an `epi` column and are left as-is)
  if(!("epi" %in% names(df))){
    key_cols <- intersect(
      c("id_scenario", "id_sim", "pt", "time", "age"),
      names(df)
    )
    df <- tidyr::pivot_longer(
      df,
      cols = -dplyr::all_of(key_cols),
      names_to = "epi",
      values_to = "value"
    )
  }

  df <- df |> dplyr::inner_join( # standardized outcome names
    lookup_outcome(),
    by = dplyr::join_by(epi == !!rlang::sym(ff$model))) |>
    dplyr::mutate(
      model = !!ff$model,
      id_scenario = !!ff$id_scenario
    ) |>
    dplyr::mutate(value = value/pop.factor)

  # aggregate across province/territory unless disaggregation is requested
  if(has_pt && !by_pt){
    df <- df |>
      dplyr::group_by(dplyr::across(-c(pt, value))) |>
      dplyr::summarize(value = sum(value), .groups = "drop")
  }

  if(has_pt && by_pt){
    df |> dplyr::select(
      id_scenario, model, outcome, time, id_sim, pt, age, value
    )
  } else {
    df |> dplyr::select(
      id_scenario, model, outcome, time, id_sim, age, value
    )
  }
}
