
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


