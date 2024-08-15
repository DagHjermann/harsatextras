#' Split a time series object into a list of time series objects
#'
#' @param object A time series object resulting from \code{harsat::create_timeseries()}. The object should be a list with 6 objects, named \code{call}, \code{call.data},
#' \code{info}, \code{data}, \code{stations} and \code{timeSeries}
#' @param df_branching A data frame containing (at least) 7 columns. The six columns that will be matched with the data are "determinand",
#' "station_code", "species", "matrix", "subseries", and "basis", and they must have exactly these names. The name of the last column, which will be used for grouping,
#' is given by \code{groupcolumn}; the default name is "branch"
#' @param groupcolumn A string with the name of the column that shall be used for splitting
#' @return Returns a list of time series objects, each of which can be input for run_assessment_tar()
#' @export
#'
#' @examples
#'
#' # Original time series object: a list of 6 elements
#' str(norway_timeseries_all, 1)
#'
#' # Split it, using 'norway_branching_groups':
#' timeseries_list <- split_timeseries_object(norway_timeseries_all, norway_branching_groups)
#'
#' # The result is a list of two time series objects:
#' str(timeseries_list, 2)
#'
#'
split_timeseries_object <- function(object, df_branching, groupcolumn = "branch"){
  # Add 'groupcolumn' (usually column named 'branch') to the data, based on the 'df_branching' table
  object$data <- base::merge(
    object$data,
    df_branching,
    by = c("determinand", "station_code", "species", "matrix", "subseries", "basis"),
    all.x = TRUE, all.y = FALSE)
  # Remove rows with missing values in 'groupcolumn'
  sel_missing <- is.na(object$data[[groupcolumn]])
  object$data <- object$data[!sel_missing,]
  # Get groupings (tar_group_data) and unique values of groupings (tar_groups)
  tar_group_data <- object$data[[groupcolumn]]
  tar_groups <- unique(tar_group_data)
  # split data into a list
  data_list <- lapply(tar_groups, function(tar_group) { object$data[tar_group_data %in% tar_group,] })
  names(data_list) <- tar_groups
  # split time series into a list
  timeSeries_list <- vector("list", length = length(data_list))
  for (i in 1:length(data_list)){
    # initialize 'sel' vector to use for selecting rows in the timeSeries_list
    sel <- rep(TRUE, nrow(object$timeSeries))
    # columns <- names(object$timeSeries)
    columns <- c("station_code", "determinand", "species", "matrix", "subseries")
    # go through each column to modify 'sel' so it fit's what's in the data
    for (col in columns){
      group_values <- unique(data_list[[i]][[col]])
      if (sum(!is.na(group_values)) > 0){
        sel <- sel & object$timeSeries[[col]] %in% group_values
      }
    }
    timeSeries_list[[i]] <- object$timeSeries[sel,]
  }
  names(timeSeries_list) <- tar_groups
  # define result as a list with one element per group
  result <- vector(mode = "list", length = length(tar_groups))
  names(result) <- tar_groups
  # define each list item
  for (i in seq_along(result)){
    result[[i]] <- list(
      call = object$call,
      call.data = object$call.data,
      # info = object$info,
      data = data_list[[i]],
      # data = object$data,
      stations = object$stations,
      timeSeries = timeSeries_list[[i]]
    )
  }
  result
}
