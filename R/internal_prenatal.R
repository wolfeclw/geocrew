
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
pre_means_i <- function(l, ints) {

  l_empty <- purrr::map_lgl(l, ~nrow(.) == 0)

  d_int_not_empty <- l[!l_empty]
  avail_ints <- ints[!l_empty]


  pre_means_by_int <- function(d_int, time_unit) {

    tu <- stringr::str_split(time_unit, '_')
    tu <- purrr::map_chr(tu, 1)

    d_na <- d_int %>%
      dplyr::group_by(subjectid, dplyr::across({{time_unit}})) %>%
      dplyr::mutate(dplyr::across(c(pm25, no2, o3), ~max(dplyr::row_number()),
                           .names = "{col}_{time_unit}_n"),
                    dplyr::across(c(pm25, no2, o3), ~sum(is.na(.)), .names = '{col}_na_n'))

    v <- v <- names(d_na)

    v_int <- v[stringr::str_detect(v, 'int_n')]
    v_na <- v[stringr::str_detect(v, 'na_n')]

    d_na <- purrr::map2(v_na, v_int, ~dplyr::mutate(d_na, dplyr::across(.x, ~ (.data[[.y]] - . )/ .data[[.y]], .names = '{col}_pct')))
    d_na <- suppressMessages(purrr::reduce(d_na, dplyr::inner_join))

    d_avg <- d_na %>%
      dplyr::mutate(pm25 = ifelse(pm25_na_n_pct < 0.75, NA, pm25),
             no2 = ifelse(no2_na_n_pct < 0.75, NA, no2),
             o3 = ifelse(o3_na_n_pct < 0.75, NA, o3)) %>%
      dplyr::select(-dplyr::ends_with('na_n_pct')) %>%
      dplyr::rename_with(~stringr::str_replace_all(., '_int_n', '_n')) %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with(c('pm25', 'no2', 'o3')), mean, .names = '{.col}_{tu}'),
                       .groups = 'drop') %>%
      dplyr::ungroup() %>%
      dplyr::select_if(~!lubridate::is.interval(.x)) %>%
      dplyr::rename_with(~stringr::str_replace_all(., paste('n', tu, sep = '_'), 'n'))
  }

  purrr::map2(d_int_not_empty, avail_ints, ~pre_means_by_int(.x, .y))
}
