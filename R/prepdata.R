

#
# t_data %>%
#   group_nest(id, dob, .key = 'air_data') %>%
#   filter(map(.$air_data, ~PM25 < 1))




t <- t_data %>%
  arrange(id, date) %>%
  filter(id %in% c(100027, 100112))
#group_by(id, date)

# t %>%
#   mutate(interval_months = lubridate::interval(date, dob),
#          month_since_birth = interval_months %/% months(1))


prepdata <- function(d) {

  if (!'gest_age' %in% names(d)) {
    message('Could not find column `gest_age`. This column has been added to the export data frame. ')
    d$gest_age <- NA
  }

  if (dplyr::is.grouped_df(d)) {
    d <- ungroup(d)
  }

  f_data <- d %>%
    dplyr::arrange(id, date) %>%
    dplyr::group_split(id) %>%
    purrr::map(.,
        ~filter(., !date < (dob - lubridate::dyears(1)))) %>%
    purrr::map_df(.,
           ~dplyr::mutate(.,
                   O3 = ifelse(O3 == 0, NA, O3),
                   # interval_date = lubridate::interval(dob, date - lubridate::ddays(1)),
                   interval_date = lubridate::interval(dob, date),
                   # month_since_birth = interval_date %/% months(1),
                   # month_since_birth = lubridate::month(lubridate::as.period(interval_date)),
                   # month_since_birth = ifelse(month_since_birth == 0 & sign(dplyr::lead(month_since_birth)) == -1,
                   #                                                          lead(month_since_birth), month_since_birth),
                   # month_since_birth = ifelse(date < dob, month_since_birth - 1, month_since_birth),
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
  # maxmon = as.numeric(round((date - dob)/365.25 * 12, digits = 0))
                # maxmon = as.numeric(floor((date - dob)/365.25 * 12))

  n_data <- f_data %>%
    dplyr::group_split(id) %>%
    purrr::map_df(.,
                  ~dplyr::mutate(.,
                          f_date_id = first(date),
                          l_date_id = last(date),
                          b_days = as.numeric(lubridate::as.duration(lubridate::interval(f_date_id, date)), 'days'),
                          e_days = as.numeric(lubridate::as.duration(lubridate::interval(date, l_date_id)), 'days'))) %>%
    dplyr::group_split(id, month_since_birth) %>%
    purrr::map_df(.,
                  ~dplyr::mutate(.,
                           b_max = max(b_days),
                           e_max = max(e_days),
                           f_date = first(date),
                           l_date = last(date),
                           interval_max = lubridate::interval(f_date, l_date),
                           lub_days = lubridate::days(l_date - f_date),
                           lub_days = lubridate::day(lub_days) + 1,
                           N_days_month = ifelse(b_max < lub_days | e_max < lub_days,
                                                 lubridate::days_in_month(date), lub_days)))  %>%
    dplyr::group_split(id, year_since_birth) %>%
    map_df(.,
           ~mutate(.,
                   yr_interval = lubridate::interval(dplyr::first(date), dplyr::last(date)),
                   N_days_year = as.numeric(lubridate::as.duration((yr_interval)), 'days') + 1,
                   N_days_year = ifelse(N_days_year < 365, 365, N_days_year))) %>%
    dplyr::select(id, dob, gest_age, date, PM25, NO2, O3, N_days_month, month_since_birth, N_days_year, year_since_birth)
    # dplyr::select(-c(f_date_id, l_date_id, b_days, e_days, b_max, e_max, f_date, l_date, interval_max, lub_days,
    #                  yr_interval))

  n_data
}

