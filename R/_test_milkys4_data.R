
#
# Script for testing app
#
# NOTE: also used for making plots for national environmental indicator (project "miljogift-indikator-R")

library(devtools)
assess <- readRDS("/home/jovyan/shared/common/DHJ/milkys4/other/OSPAR_NO_2023_assessment_for_app_2024-11-22.rds")
trends <- readRDS("/home/jovyan/shared/common/DHJ/milkys4/harsat_reports/milkys4_903_trends_12.rds")
load_all()
library(dplyr)

trends %>%
  count(series) %>%
  filter(n == 2)

open_assessment_app(assess, trends)


# debugonce(ggplot_assessment)
# debugonce(get_trend_text)
ggplot_assessment(
  assess[["10921 CD Gadus morhua LI NA"]],
  subset(trends, series %in% "10921 CD Gadus morhua LI NA"),
  add_trend_text = TRUE)



#
# TEST open_assesment_app ----
#

#
# . - before ui and server ----
#

# use the names used in the function
assessdata_object <- assess
trend_dataframe <- trends

series <- purrr::map(assessdata_object, "series")
series_names <- names(series)

lookup_series <- data.frame(
  names = names(series),
  group = purrr::map_chr(series, "group"),
  determinand = purrr::map_chr(series, "determinand"),
  station_name = purrr::map_chr(series, "station_name"),
  station_code = purrr::map_chr(series, "station_code"),
  matrix = purrr::map_chr(series, "matrix")
) |>
  dplyr::mutate(station = paste0(station_name, " (", station_code, ")"))

lookup_determinands <- lookup_series |>
  dplyr::distinct(group, determinand)

lookup_stations <- lookup_series |>
  dplyr::distinct(station_code, station_name, station)

#
# . - ui ----
#

input <- list()
input$determinand <- "HG.LENADJ"
input$station <- "30B Oslo City area (4684)"
input$matrix <- "MU"
input$plot_points <- "Annual means"
input$logscale <- "Linear scale"
input$plot_title <- "Mercury (Hg) in cod (Gadus morhua) muscle"
input$plot_subtitle <- "Station 30B Inner Oslofjord"
input$plot_ylabel <- "Concentration, \u03BCg/kg (w.w.) for 50 cm cod"
input$add_trend_text <- TRUE


#
# . - server ----
#

# select by determinand
stations_from_determ <- # reactive({

  subset(lookup_series, determinand %in% input$determinand)$station |> unique()

# })

# output$stationControls <- shiny::renderUI
# - we skip this one

# select by determinand and station
series_determ_station <- # reactive({

  subset(lookup_series, determinand %in% input$determinand & station %in% input$station)

#  selected_series
# })

# output$matrixControls <- shiny::renderUI({
# - we skip this one too


# output$trendplot <- shiny::renderPlot({

  selected_matrix <- input$matrix

  selected_series <- subset(
    series_determ_station,        # parantheses removed from 'series_determ_station()'
    matrix %in% selected_matrix)

  if (nrow(selected_series) > 1){
    warning("more than one series selected")
  }

  seriesname <- selected_series$name[1]

  if (input$plot_points == "Annual means"){
    plot_points <- "annual"
  } else if (input$plot_points == "All data"){
    plot_points <- "all"
  }

  if (input$logscale == "Log scale"){
    logscale <- TRUE
  } else if (input$logscale == "Linear scale"){
    logscale <- FALSE
  }

  if (input$plot_title != ""){
    title <- input$plot_title
  } else {
    title <- NULL
  }

  if (input$plot_subtitle != ""){
    subtitle <- input$plot_subtitle
  } else {
    subtitle <- NULL
  }

  if (input$plot_ylabel != ""){
    label_yaxis <- input$plot_ylabel
  } else {
    label_yaxis <- NULL
  }

  # browser()

  # . - basic plot ----

  gg <- ggplot_assessment(
    assessdata_object[[seriesname]],
    trend_dataframe_series = subset(trend_dataframe, series %in% seriesname),
    plot_points = plot_points,
    logscale = logscale,
    add_trend_text = input$add_trend_text,
    title = title,
    subtitle = subtitle,
    label_yaxis = label_yaxis
  )


  # if (input$expand_x_txt != ""){
  #   expand_x <- as.numeric(strsplit(input$expand_x_txt, ",")[[1]])
  #   gg <- gg + ggplot2::expand_limits(x = expand_x)
  # }
  #
  # if (input$expand_y_txt != ""){
  #   expand_y <- as.numeric(strsplit(input$expand_y_txt, ",")[[1]])
  #   gg <- gg + ggplot2::expand_limits(y = expand_y)
  # }

  gg

# })#
# }

#
# . - plot with extras, Hg in cod ----
#

species <- "Gadus morhua"

# get proref
  lookup_proref <- read.csv("../Milkys2/Input_data/Lookup_tables/Lookup_proref.csv")
proref_sel <- lookup_proref %>%
  filter(PARAM == "HG" & Basis == "WW" & LATIN_NAME  == species)
proref_sel

lookup_sylvia <- read.csv("../miljogift-indikator-R/data_input/lookup/Grenseverdier_fra_Sylvia.csv")
lookup_sylvia_sel <- lookup_sylvia %>%
  filter(PARAM == "HG" & LATIN_NAME  == species)
lookup_sylvia_sel$Mattrygghet

# change plot
library(ggplot2)

eqs <- 20
proref <- proref_sel$Proref*1000   # because of unit
foodlimit <- lookup_sylvia_sel$Mattrygghet
line_label_x <- 1976

gg2 <- gg +
  scale_x_continuous(limits = c(1976, 2025)) +
  scale_y_continuous(limits = c(0,390)) +
  geom_hline(yintercept = eqs, linetype = "dashed",
             color = "red3") +
  annotate("text", x = line_label_x, y = eqs, label = "EQS",
           color = "red3", vjust = -0.3, hjust = 0) +
  geom_hline(yintercept = proref, linetype = "dashed",
             color = "darkgreen") +
  annotate("text", x = line_label_x, y = proref, label = "PROREF",
           color = "darkgreen", vjust = -0.3, hjust = 0) +
  geom_hline(yintercept = 2*proref, linetype = "dashed",
             color = "darkgreen") +
  annotate("text", x = line_label_x, y = 2*proref, label = "PROREF x 2",
           color = "darkgreen", vjust = -0.3, hjust = 0) +
  geom_hline(yintercept = 5*proref, linetype = "dashed",
             color = "darkgreen") +
  annotate("text", x = line_label_x, y = 5*proref, label = "PROREF x 5",
           color = "darkgreen", vjust = -0.3, hjust = 0)  +
  geom_hline(yintercept = foodlimit, linetype = "dashed",
             color = "purple") +
  annotate("text", x = line_label_x, y = foodlimit, label = "Mattrygghet",
           color = "purple", vjust = -0.3, hjust = 0) +
  theme_bw() +
  # title with latin name in italics, markdown format (see theme 'element_markdown' below)
  labs(title = "Mercury (Hg) in cod (*Gadus morhua*) muscle") +
  theme(plot.title = ggtext::element_markdown())

gg2

ggsave("../milkys4/other/miljøindikator2025_hg_torsk.png", gg2, width = 8.3, height = 5.5)



#
# . - plot with extras, Hg in blue mussel ----
#

input$determinand <- "HG"
input$station <- "I301 Akershuskaia (5030)"
input$matrix <- "SB"
input$plot_title <- "Mercury (Hg) in blue mussel (Mytilus edulis) whole soft body"
input$plot_subtitle <- "Station I301 Akershuskaia, Inner Oslofjord"
input$plot_ylabel <- "Concentration, \u03BCg/kg (w.w.)"

#
# RUN "server" and "basic plot"
#


species <- "Mytilus edulis"

# get proref
lookup_proref <- read.csv("../Milkys2/Input_data/Lookup_tables/Lookup_proref.csv")
proref_sel <- lookup_proref %>%
  filter(PARAM == "HG" & Basis == "WW" & LATIN_NAME  == species)
proref_sel

lookup_sylvia <- read.csv("../miljogift-indikator-R/data_input/lookup/Grenseverdier_fra_Sylvia.csv")
lookup_sylvia_sel <- lookup_sylvia %>%
  filter(PARAM == "HG" & LATIN_NAME  == species)
lookup_sylvia_sel$Mattrygghet


# change plot
library(ggplot2)

eqs <- 20
proref <- proref_sel$Proref*1000   # because of unit
foodlimit <- lookup_sylvia_sel$Mattrygghet
line_label_x <- 1985

gg2 <- gg +
  scale_x_continuous(limits = c(1985, 2025)) +
  scale_y_continuous(limits = c(0,37)) +
  geom_hline(yintercept = eqs, linetype = "dashed",
             color = "red3") +
  annotate("text", x = line_label_x, y = eqs, label = "EQS",
           color = "red3", vjust = -0.3, hjust = 0) +
  geom_hline(yintercept = proref, linetype = "dashed",
             color = "darkgreen") +
  annotate("text", x = line_label_x, y = proref, label = "PROREF",
           color = "darkgreen", vjust = -0.3, hjust = 0) +
  theme_bw() +
  # title with latin name in italics, markdown format (see theme 'element_markdown' below)
  labs(title = "Mercury (Hg) in blue mussel (*Mytilus edulis*) whole soft body") +
  theme(plot.title = ggtext::element_markdown())

gg2

ggsave("../milkys4/other/miljøindikator2025_hg_blåskjell.png", gg2, width = 8.3, height = 5.5)

