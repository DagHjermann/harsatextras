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


get_trend_text <- function(assessment_data, trenddata, trendlength){
  recent_no_years <- assessment_data$assessment$contrasts$end[2] - assessment_data$assessment$contrasts$start[2] + 1
  if (trendlength == "overall"){
    result <- "Long-term:"
    name_pvalue <- "p_overall_change"
    name_trend <- "overall_change"
    trend_string_table <- subset(trenddata, Trend_type %in% "long")$Trend_string
  } else if (trendlength == "recent"){
    result <- paste("Last", recent_no_years, "years:")
    name_pvalue <- "p_recent_change"
    name_trend <- "recent_change"
    trend_string_table <- subset(trenddata, Trend_type %in% "short")$Trend_string
  } else {
    stop("trendlength must be 'overall' or 'recent'")
  }
  pvalue <- assessment_data$assessment$summary[[name_pvalue]]
  if (trend_string_table %in% "No change"){
    result <- paste0(result, " no overall trend (", get_pvalue_text(pvalue), ")")
  } else if (trend_string_table %in% c("data span <= 10 years", "Too few years")){
    result <- paste0(result, " ", trend_string_table)
  } else if (trend_string_table %in% "Increasing"){
    result <- paste0(result, " increased concentrations (", get_pvalue_text(pvalue), ")")
  } else if (trend_string_table %in% "Decreasing"){
    result <- paste0(result, " decreased concentrations (", get_pvalue_text(pvalue), ")")
  } else {
    result <- paste0(result, " trend analysis failed")
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

