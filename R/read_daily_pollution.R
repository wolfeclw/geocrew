

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

  d_poll_names <- names(d_poll)

  if (!'subjectID' %in% d_poll_names) {
  m_names <- menu(names(d_poll), title = paste0('Column `subjectID` is not in the input data frame.',
                                          ' Please select the column that represents the participant ID.'))
  id_rename <- names(d_poll)[m_names]

  dplyr::rename(d_poll, subjectID = all_of(id_rename))

  message(paste0('Column `', id_rename, '` has been renamed to `subjectID`.'))
  }

  if ('date' %in% d_poll_names) {

    dt_names <- menu(names(d_poll), title = paste0('Column `date` is not in the input data frame.',
                                                   ' Please select the column that represents the date for each pollution measurement.'))
  }

  # if (sum(purrr::map_lgl(d_poll, lubridate::is.Date)) == 0) {
  #
  #   dt_names <- menu(names(d_poll), title = 'Column ')
  # }

  # date_guess <- lubridate::guess_formats(d_poll$date, orders = c('ymd', 'mdy', 'dmy'))
  # date_guess <- unique(stringr::str_replace_all(date_guess, '[[:punct:]O]', ''))
  #
  # date_guess

}

read_daily_pollution(pm_path, n_lines = 10)
