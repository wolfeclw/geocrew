
#' Execute averaging script by specific interval
#'
#' @param input_path character; path of input data frame
#' @param export_path character; path used to export output data
#' @param average_type character; one or combination of 'month', 'year', 'prenatal', or 'all'.
#' Default = 'all'.
#' @param cohort character; cohort name
#' @param na_SAS logical; if TRUE, missing values will be replaced with '.' instead of 'NA.'
#' Default = FALSE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' run_averages(input_path = NULL, export_path = NULL, average_type = 'all', cohort = NULL)
#' }
run_averages <- function(input_path = NULL, export_path = NULL, average_type = 'all', cohort = NULL, na_SAS = FALSE) {


  if (!dir.exists(export_path)) {
    stop(paste('Directory', crayon::cyan(export_path), 'does not exist. \n Check the export path.'),
         call. = FALSE)
  }
  pipe_file <- system.file('run', 'run_averages_script.R', package = 'geocrew')

  avg_param <- list(input_path = input_path,
                    export_path = export_path,
                    average_type = average_type,
                    cohort = cohort,
                    na_SAS = na_SAS)


  options <- getOption("geocrew")
  options[['geocrew']] <- avg_param
  class(options) <- "averaging_parameters"
  options(geocrew = options)
  invisible(NULL)

  source(pipe_file)

}
