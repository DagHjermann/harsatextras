#' Time series object resulting from harsat::create_timeseries()
#'
#' A small example time series object, containing data for four Norwegian
#' time series of contaminants in biota:
#' two series for cadmium (determinant = 'CD') and two for PFOS
#' (determinand = 'PFOS'). This object is the output of harsat::create_timeseries(),
#' and can be used as input for harsat::run_assessment. It can also be the input of
#' split_timeseries_object() in this package
#'
#' @format A list with six elements: \code{call}, \code{call.data},
#'   \code{info}, \code{data}, \code{stations} and \code{timeSeries}.
"norway_timeseries_all"

#' Data frame defining groups for static branching
#'
#' A small example data frame of four rows, containing data that defines how the four
#' time series of 'norway_timeseries_all' should be divided into groups. \code{split_timeseries_object} requires such a file
#' to define how the times series object should be split. In this case, the column 'branch' has two values
#' 'CD_allstations' and 'PFOS_allstations', meaning that the time series object will be split into two groups, one with all CD (cadmium) tiem series
#' and one with all PFOS time series.
#'
#' @format A data frame with 7 columns, six columns that will be matched with the data ("determinand",
#' "station_code", "species", "matrix", "subseries", and "basis"), and one column that defines the groupings ("branch").
"norway_branching_groups"


#' Information object used by harsat procedure
#'
#' This object contains a lot of information needed for assessment, including standard units, transformation to be used etc. for each determinand, standard dry weights
#' and lipid percentages for different tissues in each species, etc. This list is used in the tidying of the data as well as in
#' the assessment.
#'
#' @format a list of 17 objects, including strings, lists and data frames
"info_object"

