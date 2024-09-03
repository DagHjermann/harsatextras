
#
# Installing harsat ----
#
# harsat package:
#   cloned from
# devtools::install("../HARSAT")
# Installing 9 packages: bdsmatrix, fastGHQuad, bbmle, rstpm2, muhaz, mstate, TeachingDemos, optimx, flexsurv
library(harsat)

#
# Run pipeline commands ----
#

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
  determinands = harsat::ctsm_get_determinands(norway_tidy2$info),
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

plotdat <- get_assessment_data(assessment_part1)

str(plotdat, 1)
str(plotdat[[1]], 1)


library(ggplot2)

i <- 2
ggplot(plotdat[[i]]$assessment$fullData, aes(year)) +
  geom_ribbon(
    data = plotdat[[i]]$assessment$pred,
    aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
    fill = "lightblue") +
  geom_path(
    data = plotdat[[i]]$assessment$pred,
    aes(y = exp(fit))) +
  geom_point(
    aes(y = concentration, color = censoring),
    color = "darkred") +
  scale_y_log10() +
  labs(title = plotdat[[i]]$output_id)


#
# Read directly from targets results ----
#
library(devtools)
load_all()          # load 'harsatextras' package
library(targets)

#
# Load assessment data
#
dat_list <- combine_assessment_data(store = "../milkys4/_targets/")
length(dat_list)

#
# Open Shiny app
#
open_assessment_app(dat_list)

#
# Check assessment data object
#
grep("4871 CD", names(dat_list), value = TRUE)
dat <- dat_list[["4871 CD Gadus morhua LI NA"]]
str(dat, 1)
str(dat$assessment, 1)
dat$assessment$anova
dat$assessment$coefficients
dat$assessment$summary

#
# Get trend text
#
get_trend_text(dat, "overall")
get_trend_text(dat, "recent")

#
# Add trend text to ggplot
#
library(ggplot2)
ggplot_assessment(dat)
ggplot_assessment(dat, add_trend_text = TRUE)

#
# Check assessment (not 'assessment data') object
#
targts_all <- tar_completed(store = "../milkys4/_targets/")
targts_all
grep("biota_assess_data", targts_all, value = TRUE) |> head(20)
ass1 <- tar_read(biota_assessment_CD_Gad_W, store = "../milkys4/_targets/")
ass2 <- tar_read(biota_assessment_CD_Myt_W, store = "../milkys4/_targets/")
test1 <- purrr::map(list(ass1, ass2), get_assessment_data)
combine_assessment_data
names(test1[[1]])
names(test1[[2]])
test2 <- unlist(test1, recursive = FALSE)
names(test2)
x <- test2[["4871 CD Gadus morhua LI NA"]]
str(x, 1)
str(x$assessment, 1)
str(x$assessment$summary, 1)


