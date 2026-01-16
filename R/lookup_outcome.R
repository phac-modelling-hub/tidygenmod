#' Decode outcomes for each model into a standardized name
#'
#' Mapping between internal (standardized) outcome label, labels for each generic model, and a human-readible label (e.g., for plotting)
#'
#' @returns (tibble) A lookup table of outcomes
#' @export
#'
#' @examples lookup_outcome()
lookup_outcome <- function(){
  tibble::tribble(
    ~outcome, # internal outcome variable
    ~abm, # as labelled in ABM files
    ~epac, # as labelled in EPAC files
    "incidence", "Infections", "incidence",
    "prevalence", NA, "prevalence",
    "acute_care_admission", "Hospitalisations", "acute care admission",
    "acute_care_occupancy", NA, "acute care occupancy",
    "critical_care_admission", "ICUs", "critical care admission",
    "critical_care_occupancy", NA, "critical care occupancy",
    "deaths_total", NA, "total deaths",
    "deaths_new", "Deaths", "new deaths",
  )
}

#' Decode standardized outcome names into plot-ready labels
#'
#' @returns (tibble) A lookup table of outcome labels
#' @export
#'
#' @examples lookup_outcome_label()
lookup_outcome_label <- function(){
  tibble::tribble(
    ~outcome, # internal outcome variable
    ~outcome_label, # outcome label for plotting
    "incidence", "Incident infections",
    "prevalence", "Prevalence",
    "acute_care_admission", "Acute care admissions",
    "acute_care_occupancy", "Acute care occupancy",
    "critical_care_admission", "Critical care admissions",
    "critical_care_occupancy", "Critical care admissions",
    "deaths_total", "Cumulative deaths",
    "deaths_new", "New deaths"
  ) |> dplyr::mutate(outcome_label = forcats::as_factor(outcome_label)) # enforce plotting order
}
