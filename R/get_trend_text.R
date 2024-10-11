#' Extract trend text from assessment data object
#'
#' @param assessment_data One list item (i.e. one time series) from a assessment data object, i.e. the output of
#'  either 'get_assessment_data' or 'combine_assessment_data'.
#' @param trendlength Must be either 'overall' or 'recent'
#'
#' @return A string (i.e., a text)
#' @export
#'
#' @examples
#' #' #' # Extract data for plotting from an assessment object
#' assessment_data <- get_assessment_data(assessment_part1)
#' # Get trend text
#' get_trend_text(assessment_data[["4994 CD Gadus morhua LI NA"]], "overall")
#' get_trend_text(assessment_data[["4994 CD Gadus morhua LI NA"]], "recent")


get_trend_text <- function(assessment_data, trendlength){
  recent_no_years <- assessment_data$info$recent.trend
  if (trendlength == "overall"){
    result <- "Long-term:"
    name_pvalue <- "pltrend"
    name_trend <- "ltrend"
  } else if (trendlength == "recent"){
    result <- paste("Last", recent_no_years, "years:")
    name_pvalue <- "prtrend"
    name_trend <- "rtrend"
  } else {
    stop("trendlength must be 'overall' or 'recent'")
  }
  pvalue <- assessment_data$assessment$summary[[name_pvalue]]
  if (pvalue > 0.05){
    result <- paste0(result, " no overall trend (", get_pvalue_text(pvalue), ")")
  } else if (assessment_data$assessment$summary[[name_trend]] > 0){
    result <- paste0(result, " increased concentrations (", get_pvalue_text(pvalue), ")")
  } else {
    result <- paste0(result, " decreased concentrations (", get_pvalue_text(pvalue), ")")
  }
  result
}

get_pvalue_text <- function(pvalue){
  if (pvalue < 0.001){
    result <- "P<0.001"
  } else if (pvalue < 0.01){
    result <- "P<0.01"
  } else if (pvalue < 0.05){
    result <- paste0("P=", round(pvalue, 3))
  } else if (pvalue > 0.05){
    result <- paste0("P=", round(pvalue, 2))
  }
  result
}

