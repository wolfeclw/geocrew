
#' Execute averaging script by specific interval
#'
#' @param input_path character; path of input data frame
#' @param export_path character; path used to export output data
#' @param average_type character; one or combination of 'month', 'year', 'prenatal', or 'all'.
#' Default = 'all'.
#' @param cohort character; cohort name
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' run_averages(input_path = NULL, export_path = NULL, average_type = 'all', cohort = NULL)
#' }
run_averages <- function(input_path = NULL, export_path = NULL, average_type = 'all', cohort = NULL) {


  pipe_file <- system.file('run', 'run_averages_script.R', package = 'geocrew')

  avg_param <- list(input_path = input_path,
                    export_path = export_path,
                    average_type = average_type,
                    cohort = cohort)


  options <- getOption("geocrew")
  options[['geocrew']] <- avg_param
  class(options) <- "averaging_parameters"
  options(geocrew = options)
  invisible(NULL)

  source(pipe_file)

}
