#' Lookup table for outcome names
#'
#' Lookup table for outcomes
#'
#' Mapping between internal (standardized) outcome label, labels for each generic model, and a human-readible label (e.g., for plotting)
#'
#' @returns (tibble) A lookup table for outcomes
#' @export
#'
#' @examples lookup_outcomes()
lookup_outcomes <- function(){
  tibble::tribble(
    ~outcome, # internal outcome variable
    ~abm, # as labelled in ABM files
    ~epac, # as labelled in EPAC files
    ~outcome_label, # outcome label for plotting
    "incidence", "Infections", "incidence", "Incident infections",
    "prevalence", NA, "prevalence", "Prevalence",
    "acute_care_admission", "Hospitalisations", "acute care admission", "Acute care admissions",
    "acute_care_occupancy", NA, "acute care occupancy", "Acute care occupancy",
    "critical_care_admission", "ICUs", "critical care admission", "Critical care admissions",
    "critical_care_occupancy", NA, "critical care occupancy", "Critical care admissions",
    "deaths_total", "Deaths", "total deaths", "Cumulative deaths",
    "deaths_new", NA, "new deaths", "New deaths"
  ) |> dplyr::mutate(outcome_label = forcats::as_factor(outcome_label)) # enforce plotting order
}
