#' Parse output filename
#'
#' Parse standardized filename from model output. Compatible with filenames of format `[[model]]_[[filetype]]_[[id_scenario]].csv`.
#'
#' Filenames may include an optional `pt` token immediately after the filetype
#' (e.g., `abm_timeseries_pt_sarscov2_I0.csv`) to flag province/territory
#' disaggregated files; this token is dropped from the returned `id_scenario`.
#'
#' @param filepath file path (or just the filename)
#'
#' @returns A list with fields model ("epac" or "abm"), file type ("timeseries" or "summary-metrics"), and scenario id (last part of filename)
#' @export
#'
#' @examples parse_filename("epac_timeseries_smallpox.csv")
parse_filename <- function(filepath){
  # extract filename from path
  filepath_split <- stringr::str_split_1(filepath, "/")
  filename <- filepath_split[length(filepath_split)] |> stringr::str_replace_all("\\.csv", "")

  # parse filename
  filename <- filename |> stringr::str_split_1("_")

  # scenario id is everything after model and filetype
  id_parts <- filename[3:length(filename)]

  # drop optional province/territory ("pt") token
  if(id_parts[1] == "pt") id_parts <- id_parts[-1]

  list(
    model = filename[1],
    filetype = filename[2],
    id_scenario = paste0(id_parts, collapse = "_")
  )
}
