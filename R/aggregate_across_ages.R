#' Aggregate value across age groups for standardized timeseries
#'
#' @param df ([tibble::tibble]) A standardized tibble output from [standardize_timeseries]
#'
#' @returns A standardized data frame with timeseries of outcome in terms of 100K individuals (aggregated across age groups)
#' @export
aggregate_across_ages <- function(df){
  df |>
    dplyr::group_by(dplyr::across(-c(age, value))) |>
    dplyr::summarize(value = sum(value), .groups = "drop")
}
