#' Get prenatal functions
#'
#' @param df a data frame created by `prepdata()`.
#'
#' @return a data frame with mean pollution measurements for prenatal intervals.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_prenatal_means(df)
#' }
get_prenatal_means <- function(df) {

  if (!'gest_age' %in% names(df)) {
    stop('Column `gest_age` not found. Did you use `prepdata()` to create the input data frame?', call. = FALSE)
  }

  if (sum(is.na(df$gest_age)) == nrow(df)) {

    message('Gestatinal age is missing for all rows. Trimester averages were not calculated.')

  } else if (sum(is.na(df$gest_age)) > 1) {

    message('Gestational age is missing for some rows of the input data frame. Prenatal averages were not calculated \n for all participants.' )
  }

  d_gest <- dplyr::filter(df, date <= dob)

  ### find prenatal intervals
  d_intervals <- find_pre_intervals(d_gest)
  int_names <- names(dplyr::select(d_intervals, dplyr::ends_with('_int')))

  l_intervals <- dplyr::group_split(d_intervals, subjectid)

  ### function to filter results to prenatal intervals
  pre_interval_i <- function(d_int) {

    purrr::map(int_names, ~dplyr::filter(d_int, lubridate::`%within%`(date, .data[[.x]]))) %>%
      purrr::map2(., int_names,  ~dplyr::select(.x, !which(int_names != .y) + length(df)))
  }

  ### filter results to prenatal intervals
  pre_intervals <- purrr::map(l_intervals, pre_interval_i)

  ### calculate means
  pre_means <- purrr::map(pre_intervals, ~pre_means_i(., int_names))
  pre_means <- suppressMessages(purrr::map_depth(pre_means, 1, ~purrr::reduce(., dplyr::inner_join))) %>%
    dplyr::bind_rows()

  if ('pm25_tri1_n' %in% names(pre_means)) {
    pre_means <- pre_means %>%
      dplyr::mutate(length_tri1 = pm25_tri1_n,
                    length_tri2 = pm25_tri2_n,
                    length_tri3 = pm25_tri3_n)
  }


  #### cosine, decade, etc

  d_etc <- d_gest %>%
    dplyr::group_by(subjectid) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(gest_age = gest_age,
                     dob_sine = sin(2*pi*lubridate::yday(dob)/365.25),
                     dob_cos = cos(2*pi*lubridate::yday(dob)/365.25),
                     decade_born = lubridate::year(lubridate::floor_date(dob, lubridate::years(10))),
                     dob_season = dplyr::case_when(lubridate::month(dob) %in% c(3, 4, 5) ~ 1,
                                                   lubridate::month(dob) %in% c(6, 7, 8) ~ 2,
                                                   lubridate::month(dob) %in% c(9, 10, 11) ~ 3,
                                                   lubridate::month(dob) %in% c(1, 2, 12) ~ 4))

  d_prenatal <- dplyr::bind_cols(pre_means, d_etc)
  d_prenatal <- d_prenatal %>%
    dplyr::select(subjectid, gest_age, length_tri1, length_tri2, length_tri3, no2_tri1, no2_tri1_n, no2_tri2,
                  no2_tri2_n, no2_tri3, no2_tri3_n, pm25_tri1, pm25_tri1_n, pm25_tri2, pm25_tri2_n, pm25_tri3,
                  pm25_tri3_n, o3_tri1, o3_tri1_n, o3_tri2, o3_tri2_n, o3_tri3, o3_tri3_n, no2_avg2, no2_avg2_n,
                  no2_avg3, no2_avg3_n, no2_avg4, no2_avg4_n, no2_avg5, no2_avg5_n, no2_avg6, no2_avg6_n, no2_avg7,
                  no2_avg7_n, no2_avg14, no2_avg14_n, no2_avg30, no2_avg30_n, no2_avg60, no2_avg60_n, no2_avg90,
                  no2_avg90_n, pm25_avg2, pm25_avg2_n, pm25_avg3, pm25_avg3_n, pm25_avg4, pm25_avg4_n, pm25_avg5,
                  pm25_avg5_n, pm25_avg6, pm25_avg6_n, pm25_avg7, pm25_avg7_n, pm25_avg14, pm25_avg14_n, pm25_avg30,
                  pm25_avg30_n, pm25_avg60, pm25_avg60_n, pm25_avg90, pm25_avg90_n, o3_avg2, o3_avg2_n, o3_avg3,
                  o3_avg3_n, o3_avg4, o3_avg4_n, o3_avg5, o3_avg5_n, o3_avg6, o3_avg6_n, o3_avg7, o3_avg7_n, o3_avg14,
                  o3_avg14_n, o3_avg30, o3_avg30_n, o3_avg60, o3_avg60_n, o3_avg90, o3_avg90_n, dob_season, dob_sine,
                  dob_cos, decade_born)

  if ('cohort' %in% names(df)) {
    d_prenatal$cohort <- unique(df$cohort)
    d_prenatal <- d_prenatal %>% dplyr::relocate(cohort)
  }

  d_prenatal

}

