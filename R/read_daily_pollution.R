
#' Read daily air pollution
#'
#' `read_daily_pollution` can read both .CSV and .xlsx formats.  Date columns for
#' `dob` and `date` are automatically parsed.  If participant ID, DOB, and measurement
#' date columns are not automatically recognized, the user will be prompted to select
#' the corresponding variables in the input data frame.
#'
#' columns are not present
#' @param path character; folder path of input data frame
#' @param n_lines numeric; number of lines of input data frame to read.  This is useful to preview the data.
#'
#' @return
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read_daily_pollution(path, n_lines = NULL)
#' }
#'
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
    d_poll <- readr::read_csv(path, n_max = read_n_lines, show_col_types = FALSE, pro = readr::show_progress())
  }

  d_poll <- dplyr::rename_with(d_poll, stringr::str_to_lower)

  poll_names <- names(d_poll)

## rename ID column

  if (!'subjectid' %in% poll_names) {
    m_names <- menu(names(d_poll), title = paste0('Column `subjectid` is not in the input data frame.',
                                                  'Please select the column that represents the participant ID.',
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

## rename date

  if (!'date' %in% poll_names) {
    m_names <- menu(names(d_poll), title = paste0('Column `date` is not in the input data frame.',
                                                  ' Please select the column that represents the daily measurement.',
                                                  '\n Select "0" to exit this menu.'))

    if (m_names == 0) {
      stop('Operation terminated. You may also manually rename the `date` column in the input data frame.',
           call. = FALSE)
    }



    id_rename <- names(d_poll)[m_names]

    d_poll <- dplyr::rename(d_poll, date = all_of(id_rename))
    poll_names <- names(d_poll)

    message(paste0('Column `', id_rename, '` has been renamed to `date`.'))
  }

## rename DOB

  if (!'dob' %in% poll_names) {
    m_names <- menu(names(d_poll), title = paste0('Column `dob` is not in the input data frame.',
                                                  ' Please select the column that represents the participant DOB.',
                                                  '\n Select "0" to exit this menu.'))

    if (m_names == 0) {
      stop('Operation terminated. You may also manually rename the DOB column in the input data frame.',
           call. = FALSE)
    }



    id_rename <- names(d_poll)[m_names]

    d_poll <- dplyr::rename(d_poll, dob = all_of(id_rename))
    poll_names <- names(d_poll)

    message(paste0('Column `', id_rename, '` has been renamed to `dob`.'))
  }

  poll_names2 <- poll_names[poll_names != 'subjectid']
  need_names <- c("dob", "date", "pm25", "no2", "o3")

  pl <- poll_names2 %in% need_names
  nl <- need_names %in% poll_names2

  no_match <- c(need_names[!nl], poll_names2[!pl])
  no_match <- no_match[no_match %in% need_names]

  if (length(no_match) != 0) {

    stop(paste0('The following column(s) are missing from the input data frame and must be included: \n \n',
                paste0('- ', '`', crayon::red(no_match), '`', collapse = '\n')),
         call. = FALSE)
  }

  no_match

  if (sum(purrr::map_lgl(d_poll$date, lubridate::is.Date)) == 0) {

    m_date <- menu(c('month/day/year', 'year/month/day'), title = paste0('Column `date` was not automatically parsed as a date field.',
                                                  ' Please select the date format.',
                                                  '\n Select "0" to exit this menu.'))

    if (m_date == 0) {
      stop('Operation terminated. Please checck the date format.',
           call. = FALSE)
    }


    if (m_date == 1) {
      d_poll$date <- lubridate::mdy(d_poll$date)
    } else if (m_date == 2) {
      d_poll$date <- lubridate::ymd(d_poll$date)
    } else {
      stop('Column `date` could not be parsed. Please check the format of `date`.')
    }

    m_date_f <- c('month/day/year', 'year/month/day')[m_date]

    message(paste0('Column `date` has been formatted to: "', m_date_f, '"'))
  }

  if (sum(purrr::map_lgl(d_poll$dob, lubridate::is.Date)) == 0) {


    m_dob <- menu(c('month/day/year', 'year/month/day'), title = paste0('Column `dob` was not automatically parsed as a date field.',
                                                                         ' Please select the date format..',
                                                                         '\n Select "0" to exit this menu.'))

    if (m_dob == 0) {
      stop('Operation terminated. Please checck the date format of `dob`.',
           call. = FALSE)
    }

    if (m_dob == 1) {
      d_poll$dob <- lubridate::mdy(d_poll$dob)
    } else if (m_dob == 2) {
      d_poll$dob <- lubridate::ymd(d_poll$dob)
    } else {
      stop('Column `date` could not be parsed. Please check the format.')
    }

    m_dob_f <- c('month/day/year', 'year/month/day')[m_dob]

    message(paste0('Column `date` has been formatted to: "', m_dob_f, '"'))
  }

  d_poll <- d_poll %>% dplyr::relocate(subjectid, dob, date, pm25, no2, o3)
  d_poll
}




