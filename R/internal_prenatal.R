
## internal penatal functions

### find prenatal intervals
find_pre_intervals <- function(df) {

  df %>%
    dplyr::mutate(tri1_int = lubridate::interval(dob - lubridate::days(gest_age),
                                                 dob - lubridate::days(gest_age) + lubridate::days(92)),
                  tri2_int = lubridate::interval(dob - lubridate::days(gest_age) + lubridate::days(93),
                                                 dob - lubridate::days(gest_age) + lubridate::days(186)),
                  tri3_int = lubridate::interval(dob - lubridate::days(gest_age) + lubridate::days(187),
                                                 dob - lubridate::days(1)),
                  avg2_int = lubridate::interval(dob - lubridate::days(2), dob - lubridate::days(1)),
                  avg3_int = lubridate::interval(dob - lubridate::days(3), dob - lubridate::days(1)),
                  avg4_int = lubridate::interval(dob - lubridate::days(4), dob - lubridate::days(1)),
                  avg5_int = lubridate::interval(dob - lubridate::days(5), dob - lubridate::days(1)),
                  avg6_int = lubridate::interval(dob - lubridate::days(6), dob - lubridate::days(1)),
                  avg7_int = lubridate::interval(dob - lubridate::days(7), dob - lubridate::days(1)),
                  avg14_int = lubridate::interval(dob - lubridate::days(14), dob - lubridate::days(1)),
                  avg30_int = lubridate::interval(dob - lubridate::days(30), dob - lubridate::days(1)),
                  avg60_int = lubridate::interval(dob - lubridate::days(60), dob - lubridate::days(1)),
                  avg90_int = lubridate::interval(dob - lubridate::days(90), dob - lubridate::days(1)))
}


### calculate means
pre_means_i <- function(l_intervals, time_unit) {

  tu <- stringr::str_split(time_unit, '_')
  tu <- purrr::map_chr(tu, 1)

  l_intervals %>%
    dplyr::group_by(subjectid, dplyr::across({{time_unit}})) %>%
    dplyr::mutate(across(dplyr::starts_with(c('pm25', 'no2', 'o3')), ~max(dplyr::row_number()),
                         .names = "{col}_{time_unit}_n")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., '_int_n', '_n')) %>%
    dplyr::summarise(dplyr::across(dplyr::starts_with(c('pm25', 'no2', 'o3')), mean, .names = '{.col}_{tu}'),
                     .groups = 'drop') %>%
    dplyr::ungroup() %>%
    dplyr::select_if(~!lubridate::is.interval(.x)) %>%
    dplyr::rename_with(~stringr::str_replace_all(., paste('n', tu, sep = '_'), 'n'))
}
