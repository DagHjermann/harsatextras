#' Assess timeseries for trends and status (for use in targets)
#'
#' Fits a model to each timeseries, test for any temporal trend and compare with
#' thresholds. This function is identical to \code{run_assessment}, but is adapted
#' for use in targets with static branching. The main difference is that this
#' version of the function reads the \code{info} bits from a separate R object,
#' not from the main time series object (\code{ctsm_ob}).
#'
#' @param ctsm_ob A HARSAT object resulting from a call to create_timeSeries
#'   (or \code{tidy_data / tidy_data2})
#' @param info Information file
#' @param subset An optional vector specifying which timeseries are to be
#'   assessed. Might be used if the assessment is to be done in chunks because
#'   of size, or when refitting a timeseries model which has not converged. An
#'   expression will be evaluated in the timeSeries component of ctsm_ob; use
#'   'series' to identify individual timeseries.
#' @param AC A character vector identifying the thresholds to be used in status
#'   assessments. These should be in the threshold reference table. Defaults to
#'   NULL; i.e. no thresholds are used.
#' @param get_AC_fn An optional function that overrides get_AC_default. See
#'   details (which need to be written).
#' @param recent_trend An integer giving the number of years which are used in
#'   the assessment of recent trends. For example, a value of 20 (the default)
#'   consider trends in the last twenty year.
#' @param parallel A logical which determines whether to use parallel
#'   computation; default = FALSE.
#' @param extra_data `r lifecycle::badge("experimental")` A named list used to
#'   pass additional data to specific assessment routines. At present it is
#'   only used for imposex assessments, where it passes two data frames called
#'   `VDS_estimates` and `VDS_confidence_limits`. Defaults to NULL, This
#'   argument will be generalised in the near future, so expect it to change.
#' @param control `r lifecycle::badge("experimental")` A list of control
#'   parameters that allow the user to modify the way the assessment is run.
#'   At present, these only include parameters involved in post-hoc power
#'   calculations, but it is intended to move other structures such as
#'   `recent_trend` here. See details (which need to be written).
#' @param ... Extra arguments which are passed to assessment_engine. See
#'   details (which need to be written).
#'
#' @details
#' The main reason for making a new 'targets' version of the \code{run_assessment} is that
#' when we make a branched 'targets' work flow, a lot of data in the 'info' component of the
#' time series object \code{ctsm_ob} would be duplicated among branches. Thus, we use a common
#' 'info' object which is used by all branches in the work flow. This object is given as the
#' second argument of \code{run_assessment_tar}.
#'
#' @return
#' Returns a harsat assessment object, which is a list with the following 6 items:
#' 1. call (a language object)
#' 1. data - a data frame
#' 1. stations - a data.frame
#' 1. timeSeries - a data.frame with one row per time series
#' 1. info - a list of 20 items
#' 1. assessment - a list with one item per time series
#'
#' @export
#'
#' @examples
#'
#' # File paths
#' info_path <- harsatextras_example("info.rds")
#' stations_path <- harsatextras_example("ICES_DOME_STATIONS_20230829_NO.csv")
#' rawdata_path <- harsatextras_example("norway_rawdata.csv")
#'
#' # Read data
#' norway_data <- read_data_tar(
#'   compartment = "biota",
#'   purpose = "OSPAR",
#'   contaminants = file,
#'   data_format = "external",
#'   filename_info = info_path,
#'   filename_stations = stations_path,
#'   filename_contaminants = rawdata_path
#'   )
#'
#' # Tidy data and create time series object
#' norway_tidy <- harsat::tidy_data(norway_data)
#' norway_tidy2 <- tidy_data2(norway_tidy)
#' norway_timeseries_all <- harsat::create_timeseries(
#'   norway_tidy2,
#'   determinands = harsat::ctsm_get_determinands(norway_tidy2$info),
#'   determinands.control = NULL,
#'   oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
#'   return_early = FALSE,
#'   print_code_warnings = FALSE,
#'   get_basis = harsat::get_basis_most_common,
#'   normalise = FALSE,
#'   normalise.control = list()
#'   )
#'
#' # Split data, using 'norway_branching_groups':
#' norway_timeseries_list <- split_timeseries_object(norway_timeseries_all, norway_branching_groups)
#'
#' # Get info file, in common for each of the parts of norway_timeseries_list
#' info <- norway_timeseries_all$info
#'
#' # Run assessment for part 1 of the data:
#' assessment_part1 <- run_assessment_tar(
#'   norway_timeseries_list[[1]],
#'   info = info,
#'   AC = NULL,
#'   get_AC_fn = NULL,
#'   recent_trend = 20,
#'   parallel = FALSE,
#'   extra_data = NULL,
#'   control = list(power = list(target_power = 80, target_trend = 10))
#'   )
#'
#' # Check result:
#' str(assessment_part1, 1)
#'
run_assessment_tar <- function (ctsm_ob, info, subset = NULL, AC = NULL, get_AC_fn = NULL,
                                recent_trend = 20L, parallel = FALSE, extra_data = NULL,
                                control = list(), ...)
{
  ctsm_ob$call <- match.call()
  ctsm_ob$info <- info
  ctsm_ob$info$recent.trend <- recent_trend
  ctsm_ob$info$AC <- AC
  ctsm_ob$info$get_AC_fn <- get_AC_fn
  if (!is.null(AC) && is.null(ctsm_ob$info$get_AC_fn)) {
    ctsm_ob$info$get_AC_fn <- get_AC[[ctsm_ob$info$compartment]]
  }
  if (any(ctsm_ob$data$group %in% "Imposex")) {
    if (is.null(extra_data)) {
      stop("`extra_data` must be supplied for imposex assessments")
    }
    ok <- c("VDS_estimates", "VDS_confidence_limits") %in%
      names(extra_data)
    if (!all(ok)) {
      stop("argument extra_data must be a list with components ",
           "VDS_estimates and VDS_confidence_limits")
    }
  }
  ctsm_ob$info$extra_data <- extra_data
  cntrl <- harsat:::run_control_default()
  cntrl <- harsat:::run_control_modify(cntrl, control)
  if (any(names(cntrl) %in% names(ctsm_ob$info))) {
    id <- names(cntrl)
    id <- id[id %in% names(ctsm_ob$info)]
    warning("\n conflict between components of ctsm_ob$info and control parameters ",
            "- results may be unexpected:\n ", paste(id, collapse = ", "),
            "\n", call. = FALSE, immediate. = TRUE)
  }
  ctsm_ob$info <- append(ctsm_ob$info, cntrl)
  ctsm_ob$assessment <- vector(mode = "list", length = nrow(ctsm_ob$timeSeries))
  names(ctsm_ob$assessment) <- row.names(ctsm_ob$timeSeries)
  ctsm_ob$call.data <- NULL
  series_id <- row.names(ctsm_ob$timeSeries)
  if (!is.null(substitute(subset))) {
    timeSeries <- tibble::rownames_to_column(ctsm_ob$timeSeries,
                                             "series")
    ok <- eval(substitute(subset), timeSeries, parent.frame())
    series_id <- timeSeries[ok, "series"]
  }
  out <- harsat:::assessment_engine(ctsm_ob, series_id, parallel = parallel,
                                    ...)
  ctsm_ob$assessment[names(out)] <- out
  ctsm_ob
}
