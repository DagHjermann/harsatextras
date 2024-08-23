#' Tidy the data a bit more
#'
#' @param data The tidied data, typically returned from \code{tidy_data()}
#'
#' @return A harsat object similar to what is returned by tidy_data, just without the columns 'country' and 'station_name' in the 'data' object
#' @export
#'
#' @examples
#'
#' # Tidy data and create time series object
#' norway_tidy <- harsat::tidy_data(norway_data)
#' # Extra tidying:
#' norway_tidy2 <- tidy_data2(norway_tidy)
#'
#' # See ?run_assessment_tar for full workflow
#'
tidy_data2 <- function(data){
  if ("country" %in% names(data$data))
    data$data$country <- NULL
  if ("station_name" %in% names(data$data))
    data$data$station_name <- NULL
  data
}
