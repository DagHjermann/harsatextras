#' Plot assessment graph using ggplot2
#'
#' @details Makes a time trend plot, showing the estimated trend line, as well as points showing either all
#' concentrations (mulitiple samples per year) or the annual mean concentration.
#'
#' @param assessment_data One list item (i.e. one time series) from a assessment data object, i.e. the output of
#'  either 'get_assessment_data' or 'combine_assessment_data'.
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
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' # Combine assessment data (from the branches of the targets work flow) and plot
#' assessment_data <- combine_assessment_data()
#' ggplot_assessment(assessment_data[["10921 CD Gadus morhua LI NA"]])
#'
#' # Plot all concentration data
#' ggplot_assessment(assessment_data[["10921 CD Gadus morhua LI NA"]], plot_points = "all")
#'
#' # Plot all concentration data, use linear y scale (not log)
#' ggplot_assessment(assessment_data[["10921 CD Gadus morhua LI NA"]], plot_points = "all", logscale = FALSE)
#'
ggplot_assessment <- function(assessment_data,
                              plot_points = "annual",
                              logscale = TRUE,
                              pointcolor = "darkred",
                              pointshapes = c(19, 6),
                              trendcolor_line = "darkblue",
                              trendcolor_fill = "lightblue",
                              trendwidth = 0.8,
                              ylim = NULL){

  requireNamespace(ggplot2)

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
  if (!is.null(pointdata)){
    gg <- ggplot2::ggplot(pointdata, aes(year)) +
      ggplot2::geom_ribbon(
        data = assessment_data$assessment$pred,
        aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
        fill = trendcolor_fill) +
      ggplot2::geom_path(
        data = assessment_data$assessment$pred,
        aes(y = exp(fit)),
        color = trendcolor_line,
        linewidth = rel(trendwidth)) +
      ggplot2::geom_point(
        aes(y = y, shape = LOQ),
        color = pointcolor) +
      ggplot2::scale_shape_manual(
        values = c("Over LOQ" = pointshapes[1], "Under LOQ" = pointshapes[2]))
  } else {
    gg <- ggplot2::ggplot(assessment_data$assessment$pred, aes(year)) +
      ggplot2::geom_ribbon(
        aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
        fill = trendcolor_fill) +
      ggplot2::geom_path(
        aes(y = exp(fit)),
        color = trendcolor_line,
        size = rel(trendwidth))
  }
  gg <- gg  +
    ggplot2::labs(
      title = assessment_data$output_id,
      subtitle = assessment_data$series$station_name,
      y = paste0("Concentration, ", assessment_data$series$unit))
  if (!is.null(ylim)){
    if (!(is.numeric(ylim) & length(ylim) == 2)){
      stop("ylim must be a vector of two numbers, e.g., 'c(0,10)'")
    }
  }
  if (logscale){
    gg <- gg + ggplot2::scale_y_log10(limits = ylim)
  } else {
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
    plot_points = "annual", logscale = FALSE)
  ggplot_assessment(
    tar_read(biota_assess_data_PFOS)[["4994 PFOS Gadus morhua LI NA"]],
    plot_points = "all", logscale = FALSE, ylim = c(0,17))
}
