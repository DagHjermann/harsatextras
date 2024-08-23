#' Combine branched assessment results to a single list
#'
#' @description
#' If static branching is used, the workflow will result in one assessment data object per target, named e.g.
#' "biota_assessment_CD_Gad_W", "biota_assessment_DDEPP_Gad_W", etc. This function finds all those targets and reads
#' them, and combines them to a single assessment object (similar to what would have resulted from a single
#' run of 'get_assessmnet_data').
#'
#' @param targetname_to_search The string to be used to search for targets, by default 'biota_assess_data'.
#'   This string will be used to find the assessment objects.
#' @param store The path of the \code{targets} store. If you are inside a targets project, this normally doesn't need
#'   to be given.
#'
#' @return Returns an assessment data object (same as returned by 'get_assessment_data'). This is a list with one item
#' per time series. Each of these items is again a new list with the following items:
#' 1. data - a data frame with the raw data
#' 1. assessment - a list of 14 items, including \code{fullData} (data frame with raw data) and \code{pred}(data frame for plotting trend lines)
#' 1. series - a data frame with 1 row
#' 1. info - a list of 20 items
#' 1. output_id - a string describing the series, e.g. "4954 Norway 30A Gressholmen CD Mytilus edulis SB NA"
#'
#' @details This could probably also have been achieved with "tar_combine".
#'
#' @export
#'
#' @examples
#'
#' # Default usage if you are in the targets project, and the assessment data targets start
#' # with 'biota_assess_data':
#' \dontrun{
#' assessment_data <- combine_assessment_data()
#' }
#'
#' \dontrun{
#' # Reading froma targets store in another project:
#' assessment_data <- combine_assessment_data(store = "../milkys4/_targets/")
#' }
#'
#'
combine_assessment_data <- function(targetname_to_search = "biota_assess_data",
                                    store = targets::tar_config_get("store")){
  df_targets_all <- targets::tar_progress(store = store)
  target_names <- grep(targetname_to_search, df_targets_all$name, value = TRUE)
  # Load all biota assessment data, as separate objects
  targets::tar_load_raw(target_names, store = store)
  # Using get to access the given objects and combine them in a list
  #    (a list of lists, actually)
  # 'sys.nframe()' is there to look for objects in the current environment,
  #   i.e. inside the function
  object_list_unflattened <- lapply(target_names, get, envir = sys.nframe())
  # Remove separate objects
  # rm(list = target_names)
  # Flatten the list of lists, to just a list
  object_list <- unlist(object_list_unflattened, recursive = FALSE)
  object_list
}
