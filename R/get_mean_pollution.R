

pd <- prepdata(t)




air_means <- function(d, time_unit = c('month', 'year')) {

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
    d$pct_comp <- round(d$mo_days/d$N_days_month, digits = 2)
  } else if (time_unit == 'year' ) {
    d$pct_comp <- round(d$yr_days/d$N_days_year, digits = 2)
  }

  d[d$pct_comp < 0.75, 'PM25'] <- NA
  d[d$pct_comp < 0.75, 'NO2'] <- NA
  d[d$pct_comp < 0.75, 'O3'] <- NA

  air_means_i <- function(d, time_unit, grp) {
    d %>%
      group_by(id, across({{grp}})) %>%
      summarise(across(c(PM25, NO2, O3), mean, .names = '{.col}_{time_unit}'),
                n_days = n())
  }

  d_mean <- air_means_i(d, time_unit = time_unit, grp = all_of(t_grp))

  n_names <- paste(c('NO2', 'PM25', 'O3'), time_unit, 'N', sep = '_')

  dn <- n_names %>%
    purrr::map_dfc(~tibble::tibble(!!. := d_mean$n_days))

  d_sumry <- bind_cols(d_mean, dn)

  d_sumry %>%
    select(id, starts_with({{time_unit}}), starts_with('NO2'), starts_with('PM25'), starts_with('O3'))

}

air_means(pd, 'month')

air_means(pd, 'year')


###


