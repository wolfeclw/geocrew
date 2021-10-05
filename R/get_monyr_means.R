
#' Calculate mean air pollution levels by a specific time interval
#'
#' @param df a data frame created by `prepdata()`
#' @param time_unit character; averaging interval ('prenatal', 'month", 'year')
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_monyr_means(df, time_unit = 'month')
#' }
#'
get_monyr_means <- function(df, time_unit = c('month', 'year')) {

  if (length(time_unit) > 1 & time_unit[1] %in% c('month', 'year')) {
    message(paste('More than one value was supplied to `time_unit`. Only the first element', time_unit[1],'will be used.'))
    time_unit <- time_unit[1]
  }

  if (time_unit == 'month') {
    t_grp <- 'month_since_birth'

  } else if (time_unit == 'year') {
    t_grp <- 'year_since_birth'

  } else {
    stop('`time_unit` must be set to either "month" or "year".',
         call. = FALSE)
  }

  if (time_unit == 'month') {
    df$pct_comp <- round(df$mo_days/df$n_days_month, digits = 2)
  } else if (time_unit == 'year' ) {
    df$pct_comp <- round(df$yr_days/df$n_days_year, digits = 2)
  }

  df[df$pct_comp < 0.75, 'pm25'] <- NA
  df[df$pct_comp < 0.75, 'no2'] <- NA
  df[df$pct_comp < 0.75, 'o3'] <- NA

  v <- c('pm25', 'no2', 'o3')
  l <- list(c('no2', 'o3'),
            c('pm25', 'o3'),
            c('pm25', 'no2'))

  l_poll <- purrr::map(l, ~dplyr::select(df, subjectid, !.)) %>%
    purrr::map(., ~dplyr::arrange(., date))

  ## find and count NA values for each pollution meausrement
  air_na_i <- function(d, time_unit, grp, poll_var) {
    d %>%
      dplyr::group_by(subjectid, dplyr::across({{grp}})) %>%
      dplyr::mutate(dplyr::across(c({{poll_var}}),
                                  ~sum(is.na(.)),
                                  .names = 'n_na'))
  }

  d_na_poll <- purrr::map2(l_poll, v, ~air_na_i(d = .x,
                                               time_unit = {{time_unit}},
                                               grp = t_grp,
                                               poll_var = .y)) %>%
    purrr::map(., dplyr::ungroup) %>%
    purrr::map(., ~dplyr::mutate(., na_pct = n_na/mo_days)) %>%
    purrr::map2(., v, ~dplyr::mutate(.x, .y = ifelse(na_pct >= 0.75, NA, .y))) %>%
    purrr::map2(., v,  ~dplyr::select(.x, {.y})) %>%
    purrr::reduce(., dplyr::bind_cols)

  d_nopoll <- dplyr::select(df, !dplyr::all_of(v))
  d_poll <- dplyr::bind_cols(d_nopoll, d_na_poll)


  ### mean function
  air_means_i <- function(d, time_unit, grp) {
    d %>%
      dplyr::group_by(subjectid, dplyr::across({{grp}})) %>%
      dplyr::summarise(dplyr::across(c(pm25, no2, o3), mean, .names = '{.col}_{time_unit}'),
                       n_days = dplyr::n()) %>%
      dplyr::ungroup()
  }

  d_mean <- air_means_i(d_poll, time_unit = time_unit, grp = all_of(t_grp))

  n_names <- paste(c('no2', 'pm25', 'o3'), time_unit, 'n', sep = '_')

  dn <- n_names %>%
    purrr::map_dfc(~tibble::tibble(!!. := d_mean$n_days))

  d_sumry <- dplyr::bind_cols(d_mean, dn)

  d_sumry <- d_sumry %>%
    dplyr::select(subjectid, dplyr::starts_with({{time_unit}}),
                  dplyr::starts_with('no2'),
                  dplyr::starts_with('pm25'),
                  dplyr::starts_with('o3'))

  if ('cohort' %in% names(df)) {
    d_sumry$cohort <- unique(df$cohort)
  }

  d_sumry
}
