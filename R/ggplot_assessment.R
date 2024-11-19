#' Plot assessment graph using ggplot2
#'
#' @details Makes a time trend plot, showing the estimated trend line, as well as points showing either all
#' concentrations (mulitiple samples per year) or the annual mean concentration.
#'
#' @param assessment_data One list item (i.e. one time series) from a assessment data object, i.e. the output of
#'  either 'get_assessment_data' or 'combine_assessment_data'.
#' @param trend_dataframe_series A data frame containing two rows per series, with the columns 'series', 'Trend_type' ('long'
#' or 'short') and 'Trend_string'
#' @param plot_points Either "annual" or "all"
#' @param logscale Boolean (TRUE/FALSE); the default is TRUE
#' @param pointcolor String (color name or hex code) giving the color of the observation points
#' @param pointshapes A vector of two integers, giving the shape of the observation points for over-LOQ data (number 1)
#'   and under-LOQ data (number 2). See ?points for the shapes corresponding to each number. The default is 19 (round dot)
#'   for over-LOQ concentrations and 6 (triangle pointing down) for under-LOQ concentrations
#' @param trendcolor_line String (color name or hex code) giving the color of the trend line
#' @param trendcolor_fill String (color name or hex code) giving the fill color of the trend confidence interval
#' @param trendwidth Number giving the line with of the trend line
#' @param ylim A vector of two numbers, giving min and max of the y axis. If NULL (the default), this is determined automatically
#' @param title Plot title (by default, \code{output_id})
#' @param subtitle Plot subtitle (by default, \code{station_name})
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' # Extract data for plotting from an assessment object
#' assessment_data <- get_assessment_data(assessment_part1)
#'
#' # Check names of time series
#' names(assessment_part1$assessment)
#'
#' # Plot one time series, showing trend and annual means, and logarithmic y axis
#' ggplot_assessment(assessment_data[["4994 CD Gadus morhua LI NA"]])
#'
#' # Showing trend only
#' ggplot_assessment(assessment_data[["4994 CD Gadus morhua LI NA"]], plot_points = NA)
#'
#' # Showing trend and raw data
#' ggplot_assessment(assessment_data[["4994 CD Gadus morhua LI NA"]], plot_points = "all")
#'
#' # Same, but using linear logarithmic y axis
#' ggplot_assessment(assessment_data[["4994 CD Gadus morhua LI NA"]], plot_points = "all", logscale = FALSE)
#'
#' # Add custom title and subtitle
#' ggplot_assessment(assessment_data[["4994 CD Gadus morhua LI NA"]], plot_points = "all",
#'                   title = 'Cadmium in cod liver at Bjørneroya station',
#'                   subtitle = 'Note: logarithmic y axis')
#'
#' # Combine assessment data (from the branches of the targets work flow) and plot
#' \dontrun{
#' assessment_data <- combine_assessment_data()
#' ggplot_assessment(assessment_data[["10921 CD Gadus morhua LI NA"]])
#'
#' # Plot all concentration data
#' ggplot_assessment(assessment_data[["10921 CD Gadus morhua LI NA"]], plot_points = "all")
#'
#' # Plot all concentration data, use linear y scale (not log)
#' ggplot_assessment(assessment_data[["10921 CD Gadus morhua LI NA"]], plot_points = "all", logscale = FALSE)
#' }
#'
ggplot_assessment <- function(assessment_data,
                              trend_dataframe_series,
                              plot_points = "annual",
                              logscale = TRUE,
                              pointcolor = "darkred",
                              pointshapes = c(19, 6),
                              trendcolor_line = "darkblue",
                              trendcolor_fill = "lightblue",
                              trendwidth = 0.8,
                              ylim = NULL,
                              title = NULL,
                              subtitle = NULL,
                              label_yaxis = NULL,
                              add_trend_text = FALSE){

  requireNamespace("ggplot2")

  if (!is.na(plot_points)){
    if (plot_points == "all"){
      pointdata <- assessment_data$assessment$fullData
      pointdata$y = pointdata$concentration
      pointdata$LOQ = ifelse(pointdata$censoring %in% "Q", "Under LOQ", "Over LOQ")
    } else if (plot_points == "annual"){
      pointdata <- assessment_data$assessment$annualIndex
      pointdata$y = exp(pointdata$index)
      pointdata$LOQ = ifelse(pointdata$censoring %in% "Q", "Under LOQ", "Over LOQ")
    } else {
      warning(
        "plot_points = 'all': plot all concentrations\n",
        "plot_points = 'annual': plot annual index",
        "plot_points = NA: plot trend only"
      )
    }
  } else {
    pointdata <- NULL
  }
  pred <- assessment_data$assessment$pred
  if (grepl("PLUS1$", assessment_data$series$determinand)){
    pred$ci.lower <- exp(pred$ci.lower) - 1
    pred$ci.upper <- exp(pred$ci.upper) - 1
    pred$fit <- exp(pred$fit) - 1
  } else if (assessment_data$series$distribution == "lognormal"){
    pred$ci.lower <- exp(pred$ci.lower)
    pred$ci.upper <- exp(pred$ci.upper)
    pred$fit <- exp(pred$fit)
  }
  if (!is.null(pointdata)){
    gg <- ggplot2::ggplot(pointdata, ggplot2::aes(year)) +
      ggplot2::geom_ribbon(
        data = pred,
        ggplot2::aes(ymin = ci.lower, ymax = ci.upper),  # note; hard-coded exp
        fill = trendcolor_fill) +
      ggplot2::geom_path(
        data = pred,
        ggplot2::aes(y = fit),
        color = trendcolor_line,
        linewidth = ggplot2::rel(trendwidth)) +
      ggplot2::geom_point(
        ggplot2::aes(y = y, shape = LOQ),
        color = pointcolor) +
      ggplot2::scale_shape_manual(
        values = c("Over LOQ" = pointshapes[1], "Under LOQ" = pointshapes[2]))
  } else {
    gg <- ggplot2::ggplot(pred, ggplot2::aes(year)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci.lower, ymax = ci.upper),  # note; hard-coded exp
        fill = trendcolor_fill) +
      ggplot2::geom_path(
        ggplot2::aes(y = fit),
        color = trendcolor_line,
        size = ggplot2::rel(trendwidth))
  }
  if (is.null(title)){
    title <- assessment_data$output_id
    if (assessment_data$series$determinand == "VDSI.PLUS1"){
      title <- sub("VDSI.PLUS1", "VDSI (imposex)", title)
    }
  }
  if (is.null(subtitle))
    subtitle <- assessment_data$series$station_name
  if (is.null(label_yaxis)){
    if (grepl("VDSI", assessment_data$series$determinand)){
      label_yaxis <- "Average imposex index"
    } else {
      label_yaxis <- paste0("Concentration, ", assessment_data$series$unit)
    }
  }
  gg <- gg  +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      y = label_yaxis)
  if (!is.null(ylim)){
    if (!(is.numeric(ylim) & length(ylim) == 2)){
      stop("ylim must be a vector of two numbers, e.g., 'c(0,10)'")
    }
  }
  if (add_trend_text){
    textline1 <- get_trend_text(assessment_data, trend_dataframe_series, "overall")
    textline2 <- get_trend_text(assessment_data, trend_dataframe_series, "recent")
    gg <- gg +
      ggplot2::annotate(
        "text",
        x = Inf, y = Inf,                                          # Inf, Inf = top right of panel
        label = paste0(textline1, "\n", textline2),                # \n means 'new line'
        hjust = 1.02, vjust = 1.3)                                 # hjust = 1 means right-adjusted
                                                                   # vjust = 1 means top-adjusted (1.3 adds extra space)
  }
  if (logscale & add_trend_text){
    gg <- gg + ggplot2::scale_y_log10(limits = ylim,
                                      expand = ggplot2::expansion(mult = c(0, 0.2)))         # add space on top, to make room for text
  } else if (logscale & !add_trend_text){
    gg <- gg + ggplot2::scale_y_log10(limits = ylim)
  } else if (!logscale & add_trend_text){
    gg <- gg + ggplot2::scale_y_continuous(limits = ylim,
                                           expand = ggplot2::expansion(mult = c(0, 0.2)))    # add space on top, to make room for text
  } else if (!logscale & !add_trend_text){
    gg <- gg + ggplot2::scale_y_continuous(limits = ylim)
  }
  gg
}

if (FALSE){
  # testing
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]])
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]],
    plot_points = "all")
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]],
    add_trend_text = TRUE)
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]],
    plot_points = "annual", logscale = FALSE)
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]],
    plot_points = "all", logscale = FALSE, ylim = c(0,17))
}
