

pm_path <- 'test_data/schwartz_final_ccaaps.xlsx'
pm_path <- 'test_data/CCAAPS_PM25_NO2_Ozone_1km_2000_2010.csv'

pm_path <- 'test_data/ccaaps_sample.csv'


pm_path <- read_csv('ccaaps_sas_output/daily_ccaaps_monyr.csv')

# write_csv(.Last.value, 'test_data/ccaaps_sample.csv')


read_daily_pollution <- function(path, n_lines = NULL) {

  p_ext <- tools::file_ext(path)

  if (!is.null(n_lines)) {
    read_n_lines <- n_lines
  } else {
    read_n_lines <- Inf
  }


  if (p_ext == 'xlsx') {
    d_poll <- readxl::read_xlsx(path, n_max = read_n_lines)
  } else if (p_ext == 'xls') {
    d_poll <- readxl::read_xls(path, n_max = read_n_lines)
  } else if (p_ext == 'csv') {
    d_poll <- readr::read_csv(path, n_max = read_n_lines, show_col_types = FALSE)
  }

  d_poll <- rename_with(d_poll, stringr::str_to_lower)

  poll_names <- names(d_poll)

  # if (!'subjectid' %in% poll_names) {
  #   m_names <- menu(names(d_poll), title = paste0('Column `subjectid` is not in the input data frame.',
  #                                                 ' Please select the column that represents the participant ID.'))
  #   id_rename <- names(d_poll)[m_names]
  #
  #   d_poll <- dplyr::rename(d_poll, subjectid = all_of(id_rename))
  #   poll_names <- names(d_poll)
  #
  #   message(paste0('Column `', id_rename, '` has been renamed to `subjectid`.'))
  # }

  if (!'subjectid' %in% poll_names) {
    m_names <- menu(names(d_poll), title = paste0('Column `subjectid` is not in the input data frame.',
                                                  ' Please select the column that represents the participant ID.',
                                                  '\n Select "0" to exit this menu.'))

    if (m_names == 0) {
      stop('Operation terminated. You may also manually rename the ID column in the input data frame.',
           call. = FALSE)
    }



    id_rename <- names(d_poll)[m_names]

    d_poll <- dplyr::rename(d_poll, subjectid = all_of(id_rename))
    poll_names <- names(d_poll)

    message(paste0('Column `', id_rename, '` has been renamed to `subjectid`.'))
  }

  poll_names2 <- poll_names[poll_names != 'subjectid']
  need_names <- c("dob", "date", "pm25", "no2", "o3")


  # if (sum(!poll_names2 %in% need_names) != 0 | sum(!need_names %in% poll_names2) != 0) {
  #
  #   pl <- poll_names2 %in% need_names
  #   nl <- need_names %in% poll_names2
  #
  #   no_match <- c(need_names[!nl], poll_names2[!pl])
  #
  #   no_match <- no_match[no_match %in% need_names]
  #
  #   stop(paste0('The following column(s) are missing from the input data frame and must be included: \n',
  #               paste0('- ', '`', crayon::red(no_match), '`', collapse = '\n')),
  #        call. = FALSE)
  # }

  # if ('date' %in% poll_names2) {
  #
  #   dt_names <- menu(names(d_poll), title = paste0('Column `date` is not in the input data frame.',
  #                                                  ' Please select the column that represents the date for each pollution measurement.'))
  # }

  if (sum(purrr::map_lgl(d_poll$date, lubridate::is.Date)) == 0) {


    date_guess <- lubridate::guess_formats(d_poll$date, orders = c('ymd', 'mdy'))
    date_guess <- unique(stringr::str_replace_all(date_guess, '[[:punct:]O]', ''))

    if (stringr::str_to_lower(date_guess) == 'ymd') {
      d_poll$date <- lubridate::ymd(d_poll$date)
    } else if (stringr::str_to_lower(date_guess) == 'mdy') {
      d_poll$date <- lubridate::mdy(d_poll$date)
    } else {
      stop('Column `date` could not be parsed. Please check the format.')
    }
  }

  if (sum(purrr::map_lgl(d_poll$dob, lubridate::is.Date)) == 0) {


    dob_guess <- lubridate::guess_formats(d_poll$dob, orders = c('ymd', 'mdy'))
    dob_guess <- unique(stringr::str_replace_all(dob_guess, '[[:punct:]O]', ''))

    if (stringr::str_to_lower(dob_guess) == 'ymd') {
      d_poll$date <- lubridate::ymd(d_poll$dob)
    } else if (stringr::str_to_lower(dob_guess) == 'mdy') {
      d_poll$date <- lubridate::mdy(d_poll$dob)
    } else {
      stop('Column `dob` could not be parsed. Please check the format.')
    }
  }

  d_poll
}




read_daily_pollution(pm_path, n_lines = 10)
