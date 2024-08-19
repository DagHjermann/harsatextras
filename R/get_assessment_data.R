#' Extract assessment data from an assessment object
#'
#' @param assessment_obj A harsat assessment object, resulting
#'   from \code{run_assessment} or \code{run_assessment_tar}.
#' @param subset An optional vector specifying which timeseries are to be
#'   assessed. Might be used if the assessment is to be done in chunks because
#'   of size, or when refitting a timeseries model which has not converged. An
#'   expression will be evaluated in the timeSeries component of ctsm_ob; use
#'   'series' to identify individual timeseries. If you used subset when you ran
#'   \code{run_assessment}, you need to specify it here too.
#'
#' @return #' Returns a list with one item per time series. Each of these items is again a new list
#' with the following components:
#' 1. data - a data frame with the raw data
#' 1. assessment - a list of 14 items, including \code{fullData} (data frame with raw data) and \code{pred}(data frame for plotting trend lines)
#' 1. series - a data frame with 1 row
#' 1. info - a list of 20 items
#' 1. output_id - a string describing the series, e.g. "4954 Norway 30A Gressholmen CD Mytilus edulis SB NA"
#'
#' @export
#'
#' @examples
#'
#' # Exctract the assessment data
#' plotdat <- get_assessment_data(
#'   biota_assessment,
#'   subset = sel_series2)
#'
#' # Shall be equal to the number of time series:
#' length(plotdat)
#'
#' # Check time series number 1:
#' i <- 1
#' str(plotdat[[i]], 1)
#' str(plotdat[[i]]$assessment, 1)
#'
#' # Plot time series number 1 using ggplot:
#'
#' library(ggplot2)
#'
#' ggplot(plotdat[[i]]$assessment$fullData, aes(year)) +
#'   geom_ribbon(
#'     data = plotdat[[i]]$assessment$pred,
#'     aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
#'     fill = "lightblue") +
#'   geom_path(
#'     data = plotdat[[i]]$assessment$pred,
#'     aes(y = exp(fit))) +
#'   geom_point(
#'     aes(y = concentration, color = censoring),
#'     color = "darkred") +
#'   scale_y_log10() +
#'   labs(title = plotdat[[i]]$output_id)
#'
#' See ?run_assessment_tar for full workflow
#'

get_assessment_data <- function(
    assessment_obj,
    subset = NULL) {

  # silence non-standard evaluation warnings
  seriesID <- NULL

  # graphics_functions.R

  info <- assessment_obj$info
  timeSeries <- assessment_obj$timeSeries


  # set up time series information:
  # - merge with station information
  # - add in additional useful variables
  # - subset if necessary

  timeSeries <- tibble::rownames_to_column(timeSeries, "series")

  timeSeries <- dplyr::left_join(
    timeSeries,
    assessment_obj$stations,
    by = "station_code"
  )

  timeSeries$group <- ctsm_get_info(
    info$determinand,
    timeSeries$determinand,
    "group",
    info$compartment,
    sep = "_"
  )

  timeSeries$distribution <- ctsm_get_info(
    info$determinand,
    timeSeries$determinand,
    "distribution"
  )

  if (info$compartment == "water") {
    timeSeries$matrix <- "WT"
  }

  timeSeries <- harsat:::apply_subset(timeSeries, subset, parent.frame())

  series_id <- row.names(timeSeries)


  # plot each timeSeries

  result <- lapply(series_id, function(id) {

    data <- dplyr::filter(assessment_obj$data, seriesID == id)

    assessment <- assessment_obj$assessment[[id]]


    # get relevant series info

    series <- timeSeries[id, ]


    # get file name from id, and add country and station name
    # for easier identification

    output_id <- sub(
      series$station_code,
      paste(series$station_code, series$country, series$station_name),
      id,
      fixed=TRUE
    )


    # get rid of any slashes that might have crept in

    output_id <- gsub(" / ", " ", output_id, fixed = TRUE)
    output_id <- gsub("/", " ", output_id, fixed = TRUE)

    output_id <- gsub(" \ ", " ", output_id, fixed = TRUE)
    output_id <- gsub("\\", " ", output_id, fixed = TRUE)


    # plot assessment with index

    list(data=data, assessment=assessment, series=series, info=info, output_id=output_id)

  })

  names(result) <- series_id
  result

}

