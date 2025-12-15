#' Lookup table for outcome names
#'
#' Mapping between internal (standardized) outcome label, labels for each generic model, and a human-readible label (e.g., for plotting)
#'
#' @returns A tibble
#' @export
#'
#' @examples
lookup_outcomes <- function(){
  tibble::tribble(
    ~outcome, # internal outcome variable
    ~abm, # as labelled in ABM files
    ~epac, # as labelled in EPAC files
    ~outcome_label, # outcome label for plotting
    "incidence", "Infections", "incidence", "Incident infections",
    "hospital_admission", "Hospitalisations", "acute care admission", "Acute care admissions",
    "hospital_occupancy", NA, "acute care occupancy", "Acute care occupancy",
    "icu_admission", "ICUs", "critical care admission", "Critical care admissions",
    "deaths", "Deaths", "new deaths", "Cumulative deaths"
  ) |> dplyr::mutate(outcome_label = forcats::as_factor(outcome_label)) # enforce plotting order
}
