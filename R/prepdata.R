
#' Clean and prepare daily air pollution data
#'
#' @param d a data frame with daily air pollution measurements
#' @param cohort_name character; name of cohort or study
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#'prepdata(d, cohort = NULL)
#'}
#'
prepdata <- function(d, cohort_name = NULL) {

  if (!'gest_age' %in% names(d)) {
    message('Could not find column `gest_age`. This column has been added to the export data frame. ')
    d$gest_age <- NA
  }

  if (dplyr::is.grouped_df(d)) {
    d <- dplyr::ungroup(d)
  }

  f_data <- d %>%
    dplyr::arrange(subjectid, date) %>%
    dplyr::group_split(subjectid) %>%
    purrr::map(.,
        ~dplyr::filter(., !date < (dob - lubridate::dyears(1)))) %>%
    purrr::map_df(.,
           ~dplyr::mutate(.,
                   o3 = ifelse(o3 == 0, NA, o3),
                   interval_date = lubridate::interval(dob, date),
                   year_since_birth = interval_date %/% lubridate::years(1),
                   year_since_birth = ifelse(dob > date, year_since_birth - 1, year_since_birth + 1),
                   month_interval = lubridate::month(lubridate::as.period(interval_date)),
                   month_since_birth = ifelse(sign(year_since_birth) == - 1, month_interval - 1, month_interval),
                   month_since_birth = ifelse(sign(year_since_birth) == - 1 & lubridate::day(date) == lubridate::day(dob),
                                              month_since_birth + 1, month_since_birth),
                   month_since_birth = ifelse(year_since_birth > 1,
                                              month_since_birth + (12 * (year_since_birth - 1)), month_since_birth),
                   month_since_birth = ifelse(month_since_birth >= 0, month_since_birth + 1, month_since_birth))) %>%
    dplyr::select(-interval_date)

  n_data <- f_data %>%
    dplyr::group_split(subjectid) %>%
    purrr::map_df(.,
                  ~dplyr::mutate(.,
                          f_date_id = dplyr::first(date),
                          l_date_id = dplyr::last(date),
                          b_days = as.numeric(lubridate::as.duration(lubridate::interval(f_date_id, date)), 'days'),
                          e_days = as.numeric(lubridate::as.duration(lubridate::interval(date, l_date_id)), 'days'))) %>%
    dplyr::group_split(subjectid, month_since_birth) %>%
    purrr::map_df(.,
                  ~dplyr::mutate(.,
                           b_max = max(b_days),
                           e_max = max(e_days),
                           f_date = dplyr::first(date),
                           l_date = dplyr::last(date),
                           interval_max = lubridate::interval(f_date, l_date),
                           lub_days = lubridate::days(l_date - f_date),
                           lub_days = lubridate::day(lub_days) + 1,
                           n_days_month = ifelse(b_max < lub_days | e_max < lub_days,
                                                 lubridate::days_in_month(date), lub_days),
                           mo_days = dplyr::n()))  %>%
    dplyr::group_split(subjectid, year_since_birth) %>%
    purrr::map_df(.,
           ~dplyr::mutate(.,
                   yr_interval = lubridate::interval(dplyr::first(date), dplyr::last(date)),
                   n_days_year = as.numeric(lubridate::as.duration((yr_interval)), 'days') + 1,
                   n_days_year = ifelse(n_days_year < 365, 365, n_days_year),
                   yr_days = dplyr::n())) %>%
    dplyr::select(subjectid, dob, gest_age, date, pm25, no2, o3, n_days_month, month_since_birth, n_days_year, year_since_birth,
                  mo_days, yr_days)
  # %>%
  #   dplyr::distinct()
    # dplyr::select(-c(f_date_id, l_date_id, b_days, e_days, b_max, e_max, f_date, l_date, interval_max, lub_days,
    #                  yr_interval))

  if (!is.null(cohort_name)) {
    n_data$cohort <- cohort_name
    n_data <- dplyr::relocate(n_data, cohort)
    # %>% dplyr::distinct()
  }

  n_data
}

