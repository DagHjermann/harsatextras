#' Test shiny
#'
#' @return shinyApp object
#' @export
#'
#' @examples
#'
#' # Combine assessment data (from the branches of the targets work flow) and
#' # open assessment app
#' assessment_data <- combine_assessment_data()
#' open_assessment_app(assessment_data)
#'
open_assessment_app <- function(assessdata_object){

  library(ggplot2)

  series <- purrr::map(assessdata_object, "series")
  series_names <- names(series)

  lookup_series <- data.frame(
    names <- names(series),
    group = purrr::map_chr(series, "group"),
    determinand = purrr::map_chr(series, "determinand"),
    station_name = purrr::map_chr(series, "station_name"),
    station_code = purrr::map_chr(series, "station_code")
  ) |>
    dplyr::mutate(station = paste0(station_name, " (", station_code, ")"))

  lookup_determinands <- lookup_series |>
    dplyr::distinct(group, determinand)

  lookup_stations <- lookup_series |>
    dplyr::distinct(station_code, station_name, station)

  # Define UI for application that draws a histogram
  ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel("Milkys data"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(inputId = "determinand", label = "Parameter", choices = lookup_determinands$determinand, selected = "CD"),
        shiny::selectInput(inputId = "station", label = "Station", choices = lookup_stations$station, selected = "30B Oslo City area (4684)"),
        shiny::radioButtons(inputId = "plot_points", label = "Points show", choices = c("Annual means", "All data"), selected = "Annual means"),
        shiny::radioButtons(inputId = "logscale", label = "Scale of y axis", choices = c("Log scale", "Linear scale"), selected = "Log scale")
      ),

      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::plotOutput("trendplot")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    output$trendplot <- shiny::renderPlot({

      seriesname <- subset(lookup_series, determinand %in% input$determinand & station %in% input$station)$name

      if (length(seriesname) > 1){
        warning("more than one series selected")
      }

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

      seriesname <- seriesname[1]

      ggplot_assessment(
        assessment_data[[seriesname]],
        plot_points = plot_points,
        logscale = logscale)

    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
