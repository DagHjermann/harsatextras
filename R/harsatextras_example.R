#' Convenience function to make example files easy to access
#'
#' @param path A string. If \code{path} is not given, the function will return a list of the raw csv files in the package. If the name of a file is given as \code{path}, it will return the full path of the file.
#'
#' @return A list of file names of path is not given a string with the full path if path is given.
#' @export
#'
#' @examples
#'
#' harsatextras_example()
#' harsatextras_example("ICES_DOME_STATIONS_20230829_NO.csv")
#'
#' # For reading a file
#' test <- read.csv(harsatextras_example("ICES_DOME_STATIONS_20230829_NO.csv"))
#' head(test, 2)
#'
harsatextras_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "harsatextras"))
  } else {
    system.file("extdata", path, package = "harsatextras", mustWork = TRUE)
  }
}
