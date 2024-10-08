#' Modified version of harsat::read_data, for targets
#'
#' @param compartment See ?harsat::read_data
#' @param purpose See ?harsat::read_data
#' @param contaminants See ?harsat::read_data
#' @param data_format See ?harsat::read_data
#' @param filename_info Path to 'info' R object file (rds), see ?info_object
#' @param filename_stations Path to station dictionary file, csv format. See ?harsat::read_data for more
#' @param filename_contaminants Path to raw data file, csv format. See ?harsat::read_data for more
#' @param extraction See ?harsat::read_data
#' @param max_year See ?harsat::read_data
#' @param oddity_dir See ?harsat::read_data
#' @param control See ?harsat::read_data
#'
#' @return A list with four components: \code{call}, \code{info}, \code{data}, and \code{stations}
#'
#' @export
#'
#' @examples
#'
#' info_path <- harsatextras_example("info.rds")
#' stations_path <- harsatextras_example("ICES_DOME_STATIONS_20230829_NO.csv")
#' rawdata_path <- harsatextras_example("norway_rawdata.csv")
#' biota_data <- read_data_tar(
#'   compartment = "biota",
#'   purpose = "OSPAR",
#'   contaminants = file,
#'   data_format = "external",
#'   filename_info = info_path,
#'   filename_stations = stations_path,
#'   filename_contaminants = rawdata_path
#'   )
#'
read_data_tar <- function (compartment = c("biota", "sediment", "water"),
                           purpose = c("OSPAR", "HELCOM", "AMAP", "custom"),
                           contaminants,   # file name
                           data_format = c("ICES", "external"),
                           filename_info,
                           filename_stations,
                           filename_contaminants,
                           extraction = NULL,
                           max_year = NULL,
                           oddity_dir = "oddities",
                           control = list())
{
  # arguments removed (comapred to harsat::read_data:
  #   data_dir
  #   info_dir
  #   info_files
  compartment <- match.arg(compartment)
  purpose <- match.arg(purpose)
  data_format <- match.arg(data_format)
  if (!is.null(max_year)) {
    if (length(max_year) != 1) {
      stop("max_year must be a single integer valued year")
    }
    if (!is.integer(max_year)) {
      if (!isTRUE(all.equal(max_year, as.integer(max_year)))) {
        stop("max_year must be an integer valued year")
      }
      max_year <- as.integer(max_year)
    }
  }
  if (!is.null(extraction)) {
    if (length(extraction) > 1 | !is.character(extraction)) {
      stop("'extraction' must be a single character string of the form ",
           "\"yyyy-mm-dd\": e.g. \"", lubridate::today(),
           "\"", call. = FALSE)
    }
  }
  if (!is.null(extraction)) {
    extraction <- suppressWarnings(lubridate::ymd(extraction))
    if (is.na(extraction)) {
      stop("extraction not recognised: it must be a valid date of the form ",
           "\"yyyy-mm-dd\": e.g. \"", lubridate::today(),
           "\"", call. = FALSE)
    }
  }
  info <- list(compartment = compartment, purpose = purpose,
               extraction = extraction, data_format = data_format,
               max_year = max_year, oddity_dir = oddity_dir)
  cntrl <- harsat:::control_default(purpose, compartment)
  cntrl <- harsat:::control_modify(cntrl, control)
  if (any(names(cntrl) %in% names(info))) {
    id <- names(cntrl)
    id <- id[id %in% names(info)]
    warning("\n conflict between function arguments and control elements ",
            "- results may be unexpected:\n ", paste(id, collapse = ", "),
            "\n", call. = FALSE, immediate. = TRUE)
  }
  info <- append(info, cntrl)
  # info <- read_info(info, info_dir, info_files)
  info <- readRDS(filename_info)
  stations <- read_stations_tar(filename_stations, info)
  data <- read_contaminants_tar(filename_contaminants, info)
  if (data_format == "ICES") {
    data <- harsat:::add_stations(data, stations, info)
    stations <- harsat:::finalise_stations(stations, info)
    data <- harsat:::finalise_data(data, info)
  }
  if (is.null(info$max_year)) {
    info$max_year <- max(data$year)
    cat("\nArgument max_year taken to be the maximum year in the data:",
        info$max_year, "\n")
  }
  info$recent_years <- seq(info$max_year - info$reporting_window +
                             1, info$max_year)
  out <- list(call = match.call(), info = info, data = data,
              stations = stations)
  # list(info=info, stations=stations, data = data)
  out
}



# the first line "infile <- file.path(data_dir, file)" commented out
# - added "harsat:::" in front of report_file_digest, safe_read_file

read_stations_tar <- function (infile, info) {
  #  infile <- file.path(data_dir, file)
  cat("Reading station dictionary from:\n '", infile, "'\n",
      sep = "")
  if (info$data_format == "ICES") {
    var_id <- c(station_code = "character", station_country = "character",
                helcom_subbasin = "character", helcom_l3 = "character",
                helcom_l4 = "character", ices_ecoregion = "character",
                ospar_region = "character", ospar_subregion = "character",
                is_amap_area = "logical", is_helcom_area = "logical",
                is_ospar_area = "logical", station_name = "character",
                station_longname = "character", station_replacedby = "character",
                station_asmtmimeparent = "character", station_activefromdate = "character",
                station_activeuntildate = "character", station_programgovernance = "character",
                station_governance = "character", station_purpm = "character",
                station_latitude = "numeric", station_latituderange = "numeric",
                station_longitude = "numeric", station_longituderange = "numeric",
                station_geometry = "character", station_datatype = "character",
                station_wltyp = "character", station_mstat = "character",
                station_notes = "character", station_deprecated = "logical")
    harsat:::report_file_digest(infile)
    stations <- harsat:::safe_read_file(infile, sep = "\t", quote = "",
                                        na.strings = c("", "NULL"), strip.white = TRUE, colClasses = var_id)
  }
  if (info$data_format == "external") {
    var_id <- c(country = "character", station_name = "character",
                station_code = "character", station_longname = "character",
                station_latitude = "numeric", station_longitude = "numeric",
                station_type = "character", waterbody_type = "character")
    if (!is.null(info$region$id)) {
      extra <- rep("character", length(info$region$id))
      names(extra) <- info$region$id
      var_id <- c(var_id, extra)
    }
    required <- c("country", "station_name", "station_code",
                  "station_latitude", "station_longitude", info$region$id)
    harsat:::report_file_digest(infile)
    stations <- harsat:::safe_read_file(infile, strip.white = TRUE, nrows = 1)
    ok <- required %in% names(stations)
    if (!all(ok)) {
      id <- required[!ok]
      id <- sort(id)
      stop("The following variables are not in the stations file. ",
           "Please update the stations file to continue. ",
           "Note that the variable names are case sensitive.\n",
           "Variables: ", paste(id, collapse = ", "))
    }
    ok <- names(var_id) %in% names(stations)
    stations <- harsat:::safe_read_file(infile,
                                        na.strings = c("", "NULL"),
                                        strip.white = TRUE,
                                        colClasses = var_id[ok])
    id <- c("station_longname", "station_type", "waterbody_type")
    for (i in id) {
      if (is.null(stations[[i]]))
        stations[[i]] <- NA_character_
    }
    if (!all(names(var_id) %in% names(stations))) {
      stop("coding error - seek help from HARSAT team")
    }
  }
  stations
}


read_contaminants_tar <- function (infile, info){
  .data <- NULL
  # infile <- file.path(data_dir, file)
  cat("\nReading contaminant and effects data from:\n '", infile,
      "'\n", sep = "")
  if (info$data_format == "ICES") {
    var_id <- c(country = "character", mprog = "character",
                helcom_subbasin = "character", helcom_l3 = "character",
                helcom_l4 = "character", ices_ecoregion = "character",
                ospar_region = "character", ospar_subregion = "character",
                is_amap_monitoring = "logical", is_helcom_monitoring = "logical",
                is_medpol_monitoring = "logical", is_ospar_monitoring = "logical",
                is_amap_area = "logical", is_helcom_area = "logical",
                is_ospar_area = "logical", rlabo = "character", slabo = "character",
                alabo = "character", statn = "character", myear = "integer",
                date = "Date", latitude = "numeric", longitude = "numeric",
                dephu = "numeric", dephl = "numeric", purpm = "character",
                finfl = "character", param = "character", pargroup = "character",
                matrx = "character", basis = "character", value = "numeric",
                munit = "character", detli = "numeric", lmqnt = "numeric",
                uncrt = "numeric", metcu = "character", qflag = "character",
                vflag = "character", metoa = "character", metcx = "character",
                metpt = "character", metst = "character", metps = "character",
                metfp = "character", smtyp = "character", smpno = "character",
                subno = "character", dcflgs = "character", tblanalysisid = "integer",
                tblparamid = "integer", tblsampleid = "character",
                tblspotid = "integer", tbluploadid = "integer")
    if (info$compartment == "biota") {
      extra <- c(rlist = "character", speci = "character",
                 speci_name = "character", aphiaid = "integer",
                 worms_name = "character", aphiaid_accepted = "integer",
                 worms_accepted_name = "character", sexco = "character",
                 stage = "character", noinp = "integer", bulkid = "character",
                 tblbioid = "character", accessionid = "integer")
      var_id <- c(var_id, extra)
    }
    harsat:::report_file_digest(infile)
    data <- harsat:::safe_read_file(infile, sep = "\t", na.strings = c("",
                                                                       "NULL"), strip.white = TRUE, colClasses = var_id)
    if (info$compartment == "biota" && info$use_stage) {
      data$subseries <- data$stage
    }
    else {
      data$subseries <- NA_character_
    }
    data <- dplyr::mutate(data, param = toupper(.data$param),
                          munit = tolower(.data$munit))
    return(data)
  }
  if (info$data_format == "external") {
    var_id <- c(country = "character", station_code = "character",
                station_name = "character", sample_latitude = "numeric",
                sample_longitude = "numeric", year = "integer", date = "Date",
                depth = "numeric", subseries = "character", sample = "character",
                determinand = "character", matrix = "character",
                basis = "character", unit = "character", value = "numeric",
                censoring = "character", limit_detection = "numeric",
                limit_quantification = "numeric", uncertainty = "numeric",
                unit_uncertainty = "character", method_pretreatment = "character",
                method_analysis = "character", method_extraction = "character")
    if (info$compartment == "biota") {
      var_id = c(var_id, species = "character", sex = "character",
                 n_individual = "integer")
    }
    required <- c("country", "station_code", "station_name",
                  "year", "sample", "determinand", "matrix", "unit",
                  "value")
    if (info$compartment %in% c("biota", "sediment")) {
      required <- c(required, "basis")
    }
    if (info$compartment %in% c("biota")) {
      required <- c(required, "species")
    }
  }
  if (info$data_format == "external") {
    harsat:::report_file_digest(infile)
    data <- harsat:::safe_read_file(infile, strip.white = TRUE, nrows = 1)
    ok <- required %in% names(data)
    if (!all(ok)) {
      id <- required[!ok]
      id <- sort(id)
      stop("The following variables are not in the data file. ",
           "Please update the data file to continue. ",
           "Note that the variable names are case sensitive.\n",
           "Variables: ", paste(id, collapse = ", "))
    }
    ok <- names(var_id) %in% names(data)
    data <- harsat:::safe_read_file(infile, na.strings = c("", "NULL"),
                                    strip.white = TRUE, colClasses = var_id[ok])
  }
  if (info$data_format == "external") {
    id <- c("sample_latitude", "sample_longitude", "depth",
            "limit_detection", "limit_quantification", "uncertainty")
    for (i in id) {
      if (is.null(data[[i]]))
        data[[i]] <- NA_real_
    }
    id <- c("subseries", "basis", "basis", "censoring", "unit_uncertainty",
            "method_pretreatment", "method_analysis", "method_extraction")
    for (i in id) {
      if (is.null(data[[i]]))
        data[[i]] <- NA_character_
    }
    if (is.null(data$date))
      data$date <- as.Date(NA)
    if (info$compartment == "biota") {
      if (is.null(data$sex))
        data$sex <- NA_character_
      if (is.null(data$n_individual))
        data$n_individual <- NA_integer_
    }
    if (!all(names(var_id) %in% names(data))) {
      stop("coding error - seek help from HARSAT team")
    }
  }
  if (info$data_format == "external") {
    uncertainty_present <- which(complete.cases(data$uncertainty))
    uncertainty_present_valid_units <- data$unit_uncertainty[uncertainty_present] %in%
      c("%", "U2", "SD")
    if (!all(uncertainty_present_valid_units)) {
      stop("Missing or invalid uncertainty units for specified uncertainty values. ",
           "Please check that all uncertainty values have a valid unit: %, U2, or SD")
    }
  }
  if (info$data_format == "external") {
    names(data) <- tolower(names(data))
  }
  data <- dplyr::mutate(data, determinand = toupper(.data$determinand),
                        unit = tolower(.data$unit))
  data
}


