

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

  air_means_i <- function(d, time_unit, grp) {
    d %>%
      group_by(id, across({{grp}})) %>%
      summarise(across(c(PM25, NO2, O3, N_days_month), mean, .names = '{.col}_{time_unit}'),
                n_days = n()) %>%
      mutate('n_days_{time_unit}' := n_days)
  }

  air_means_i(d, time_unit = time_unit, grp = all_of(t_grp))
}

air_means(pd, 'month')


###


