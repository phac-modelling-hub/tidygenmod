#' Parse output filename
#'
#' Parse standardized filename from model output. Compatible with filenames of format `[[model]]_[[filetype]]_[[id_scenario]].csv`.
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
  scenario <-  # disease, to start

  list(
    model = filename[1],
    filetype = filename[2],
    id_scenario = paste0(filename[3:length(filename)], collapse = "_")
  )
}
