
#
# Installing harsat
#
# harsat package:
#   cloned from
# devtools::install("../HARSAT")
# Installing 9 packages: bdsmatrix, fastGHQuad, bbmle, rstpm2, muhaz, mstate, TeachingDemos, optimx, flexsurv
library(harsat)

library(devtools)
load_all()

info_path <- harsatextras_example("info.rds")
stations_path <- harsatextras_example("ICES_DOME_STATIONS_20230829_NO.csv")
rawdata_path <- harsatextras_example("norway_rawdata.csv")
branching_path <- harsatextras_example("norway_branching_groups.csv")
norway_data <- read_data_tar(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = file,
  data_format = "external",
  filename_info = info_path,
  filename_stations = stations_path,
  filename_contaminants = rawdata_path
  )
norway_tidy <- harsat::tidy_data(norway_data)
norway_tidy2 <- tidy_data2(norway_tidy)
norway_timeseries_all <- harsat::create_timeseries(
  norway_tidy2,
  determinands = harsat::ctsm_get_determinands(biota_data_tidy2$info),
  determinands.control = NULL,
  oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
  return_early = FALSE,
  print_code_warnings = FALSE,
  get_basis = harsat::get_basis_most_common,
  normalise = FALSE,
  normalise.control = list()
)

str(norway_timeseries_all, 1)

#' Split time series object, using 'norway_branching_groups':
norway_branching_groups <- read.csv(branching_path)
norway_timeseries_list <- split_timeseries_object(norway_timeseries_all, norway_branching_groups)

# The result is a list of two time series objects:
str(norway_timeseries_list, 2)

info <- norway_timeseries_all$info

assessment_part1 <- run_assessment_tar(
  norway_timeseries_list[[1]],
    info = info,
    # subset = sel_series,
    AC = NULL,
    get_AC_fn = NULL,
    recent_trend = 20,
    parallel = FALSE,
    extra_data = NULL,
    control = list(power = list(target_power = 80, target_trend = 10))
  )

# Check result:
str(assessment_part1, 1)






biota_data_tidy <- tidy_data_tar(biota_data)
# tar_target(biota_data_tidy2, tidy_data2(biota_data_tidy)),


