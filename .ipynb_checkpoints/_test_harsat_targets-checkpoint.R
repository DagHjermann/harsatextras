
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
biota_data <- read_data_tar(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = file,
  data_format = "external",
  filename_info = info_path,
  filename_stations = stations_path,
  filename_contaminants = rawdata_path
  )

# biota_data_tidy <- tidy_data_tar(biota_data)
# tar_target(biota_data_tidy2, tidy_data2(biota_data_tidy)),


