
# getwd()

norway_timeseries_all <- readRDS("data-raw/norway_timeseries_all.rds")
use_data(norway_timeseries_all)

norway_branching_groups <- readRDS("data-raw/norway_branching_groups.rds")
use_data(norway_branching_groups)

info_object <- readRDS("data-raw/info.rds")
use_data(info_object)

# station_dictionary
file.copy("data-raw/ICES_DOME_STATIONS_20230829_NO.csv", "inst/extdata/ICES_DOME_STATIONS_20230829_NO.csv")

# info object
file.copy("data-raw/info.rds", "inst/extdata/info.rds")

# assessment object
# File paths
info_path <- harsatextras_example("info.rds")
stations_path <- harsatextras_example("ICES_DOME_STATIONS_20230829_NO.csv")
rawdata_path <- harsatextras_example("norway_rawdata.csv")
# Read data
norway_data <- read_data_tar(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = file,
  data_format = "external",
  filename_info = info_path,
  filename_stations = stations_path,
  filename_contaminants = rawdata_path
)
# Tidy data and create time series object
norway_tidy <- harsat::tidy_data(norway_data)
norway_tidy2 <- tidy_data2(norway_tidy)
norway_timeseries_all <- harsat::create_timeseries(
  norway_tidy2,
  determinands = harsat::ctsm_get_determinands(norway_tidy2$info),
  determinands.control = NULL,
  oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
  return_early = FALSE,
  print_code_warnings = FALSE,
  get_basis = harsat::get_basis_most_common,
  normalise = FALSE,
  normalise.control = list()
)
# Split data, using 'norway_branching_groups':
norway_timeseries_list <- split_timeseries_object(norway_timeseries_all, norway_branching_groups)
# Get info file, in common for each of the parts of norway_timeseries_list
info <- norway_timeseries_all$info
# Run assessment for part 1 of the data:
assessment_part1 <- run_assessment_tar(
  norway_timeseries_list[[1]],
  info = info,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 20,
  parallel = FALSE,
  extra_data = NULL,
  control = list(power = list(target_power = 80, target_trend = 10))
)
use_data(assessment_part1)
