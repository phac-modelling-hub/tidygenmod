# Standardize outcomes across model outputs

#' @param filepath Path to a file
#' @param pop.factor_abm Population normalization factor for the ABM to rescale to outcomes per 100K individuals (since ABM can be run for different population sizes)
#'
#' @return A standardized data frame with outcome values in terms of 100K individuals
standardize_outcomes <- function(filepath, pop.factor_abm){
  ff <- parse_filename(filepath)
  pop.factor <- ifelse(ff$model=="abm", pop.factor_abm, 41288599/1e5)

  df <- readr::read_csv(filepath, show_col_types = FALSE)

  # rename cols to standardize for calculations below
  if(ff$model=="abm"){
    df <- (df
           |> dplyr::rename(
             id = IT, time = DAY, epi = OUTCOME, age = AGE_GRP, value = VALUE
           ))
  }

  # standardize outcome names
  df <- dplyr::right_join(df, lookup_outcomes() |> dplyr::select(-outcome_label), by = dplyr::join_by(epi == !!rlang::sym(ff$model))) |> dplyr::arrange(outcome_label)

  # final touches
  (df
    |> dplyr::mutate(model = !!ff$model, id_scenario = !!ff$id_scenario)
    |> dplyr::select(id_scenario, model, outcome, time, id, age, value)
    # aggregate across age groups
    |> dplyr::group_by(dplyr::across(-c(age, value)))
    |> dplyr::summarize(value = sum(value), .groups = "drop")
    # normalize by population factor
    |> dplyr::mutate(value = value/pop.factor)
  )
}
