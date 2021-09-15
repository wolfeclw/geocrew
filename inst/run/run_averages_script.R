

### get avaraging parameters

averaging_options <- getOption('geocrew')
averaging_options <- averaging_options$geocrew


if (sum(averaging_options$average_type %in% c('all', 'month', 'year', 'prenatal') ==  0)) {

  stop('`average_type` must be set to "all", "year", "month", "prenatal" or a combination of \n those options (i.e. "c("month", "year")',
       call. = FALSE)
}


### read pollution file

message(crayon::green('Reading the input data frame now... \n'))
Sys.sleep(3)

d_poll <- read_daily_pollution(path = averaging_options$input_path)

### prepare data

message(crayon::green('Preparing the data...'))

### guess computation time

prep_time <- (0.001*nrow(d_poll))

if (sum(averaging_options$average_type %in% c('all', 'prenatal')) > 0) {

  pre_time <- (0.00043*nrow(d_poll))
} else {
  pre_time <- 0
}

calc_time <- prep_time + pre_time
calc_time <- lubridate::seconds_to_period(calc_time)
calc_hour <- calc_time@hour
calc_mins <- calc_time@minute

calc_text <- paste(calc_hour, 'hours and', calc_mins, 'minutes')


Sys.sleep(3)
message(crayon::green(paste('Sit back and relax. By my calculations, this will take approximately', calc_text,
                            '(give or take a few).')))

###  prepare data

d_prepared <- prepdata(d_poll, cohort_name = averaging_options$cohort)


### calculate specified averages

if (sum(averaging_options$average_type == 'all') > 0) {

  message(crayon::green('Calculating averages by month since birth...'))
  Sys.sleep(3)
  d_month <- get_monyr_means(d_prepared, 'month')

  message(crayon::green('Calculating averages by year since birth...'))
  Sys.sleep(3)
  d_year <- get_monyr_means(d_prepared, 'year')

  message(crayon::green('Calculating prenatal averages...'))
  d_prenatal <- get_prenatal_means(d_prepared)

  ### write output
  readr::write_csv(d_month, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_month.csv'))
  readr::write_csv(d_year, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_year.csv'))
  readr::write_csv(d_prenatal, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_prenatal.csv'))

  readr::write_rds(d_year, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_year.rds'))
  readr::write_rds(d_month, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_month.rds'))
  readr::write_rds(d_prenatal, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_prenatal.rds'))


}

if ('year' %in% averaging_options$average_type) {

  message(crayon::green('Calculating averages by year since birth...'))
  Sys.sleep(3)

  d_year <- get_monyr_means(d_prepared, 'year')
  readr::write_csv(d_year, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_year.csv'))
  readr::write_rds(d_year, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_year.rds'))

}


if ('month' %in% averaging_options$average_type) {

  message(crayon::green('Calculating averages by month since birth...'))
  Sys.sleep(3)

  d_month <- get_monyr_means(d_prepared, 'month')
  readr::write_csv(d_month, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_month.csv'))
  readr::write_rds(d_month, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_month.rds'))

}

if ('prenatal' %in% averaging_options$average_type) {

  message(crayon::green('Calculating prenatal averages...'))

  d_prenatal <- get_prenatal_means(d_prepared)
  readr::write_csv(d_prenatal, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_prenatal.csv'))
  readr::write_rds(d_prenatal, paste0(averaging_options$export_path, '/', averaging_options$cohort, '_prenatal.rds'))


}


###

message(crayon::green('All done!'))
message(crayon::green(paste0('The exported data tables are here: ',
                             crayon::cyan(averaging_options$export_path), '.')))

rm(averaging_options, calc_hour, calc_mins, calc_text, calc_time, prep_time, pre_time)

