#' Summarize range across iterations
#'
#' @param df (tibble) Standardized timeseries
#'
#' @returns [tibble::tibble]
#' @export
summarize_range_across_iterations <- function(df){
  (df
   |> dplyr::group_by(dplyr::across(-c(id, value)))
   |> dplyr::summarize(
     med = mean(value),
     lwr = min(value),
     upr = max(value),
     .groups = "drop"
   )
  )
}

#' Summarize outcomes by peak
#'
#' @param df (tibble) Standardized timeseries
#'
#' @returns [tibble::tibble]
#' @export
summarize_outcomes_by_peak <- function(df){
  rank_lookup <- (df
    |> dplyr::group_by(dplyr::across(-c(time, value)))
    |> dplyr::summarize(value = max(value), .groups = "drop")
    |> dplyr::mutate(rank = dplyr::row_number(value))
    |> dplyr::filter(rank == min(rank) | rank == round(median(rank)) | rank ==max(rank))
    |> dplyr::arrange(rank)
    |> dplyr::mutate(type = c("lwr", "med", "upr"))
    |> dplyr::select(id_scenario, model, id, type)
   )

  (df
    |> dplyr::right_join(rank_lookup,
                        by = dplyr::join_by(id_scenario, model, id))
    |> tidyr::pivot_wider(id_cols = c("id_scenario", "model", "outcome", "time", "outcome_label"),
                          names_from = type)
  )
}

#' Summarize a grouped value
#'
#' @param df (tibble) Standardized timeseries
#' @param fun (function) Summary function
#' @param days_horizon (numeric) Calculate summary after this many days
#'
#' @returns [tibble::tibble]
#' @export
summarize_grouped_value <- function(df, fun, days_horizon){

  df |>
    # filter in time
    dplyr::filter(time <= days_horizon) |>
    # compute (grouped) summary
    dplyr::group_by(dplyr::across(-c(time, value)))
    dplyr::summarize(df,
       value = dplyr::if_else(
         any(outcome == "death"), # if else is vectorized, so need to make sure we're returning one logical to get back one value for the summary
         max(value), # cumulative deaths mean total and peak will be the largest value (at the end)
         fun(value)
       ),
       .groups = "drop")
}

#' Summarize total value
#'
#' @param df (tibble) Standardized timeseries
#' @param days_horizon (numeric) Calculate summary after this many days
#'
#' @returns [tibble::tibble]
#' @export
summarize_total <- function(df, days_horizon){
  summarize_grouped_value(df = df, fun = sum, days_horizon = days_horizon)
}

#' Summarize peak value
#'
#' @param df (tibble) Standardized timeseries
#' @param days_horizon (numeric) Calculate summary after this many days
#'
#' @returns [tibble::tibble]
#' @export
summarize_peak_value <- function(df, days_horizon){
  summarize_grouped_value(df = df, fun = max, days_horizon = days_horizon)
}

#' Summarize peak day
#'
#' @param df (tibble) Standardized timeseries
#' @param days_horizon (numeric) Calculate summary after this many days
#'
#' @returns [tibble::tibble]
#' @export
summarize_peak_day <- function(df, days_horizon){
  peak_data <- summarize_peak_value(df, days_horizon)

  suppressMessages(dplyr::right_join(df, peak_data)) |>
    dplyr::select(-value) |>
    # ensure one peak is returned (in case of repeated maxima like for cumulative deaths)
    dplyr::group_by(dplyr::across(-c(time))) |>
    dplyr::filter(time == min(time)) |>
    dplyr::ungroup() |>
    dplyr::rename(value = time) |>
    dplyr::select(dplyr::all_of(names(peak_data)))
}


