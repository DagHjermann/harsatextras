#' Test shiny
#'
#' @param assessdata_object An assessment data object, i.e. the output of \code{get_assessment_data} or \code{combine_assessment_data}.
#' @param trend_dataframe A data frame containing two rows per series, with the columns 'series', 'Trend_type' ('long'
#' or 'short') and 'Trend_string'
#' @return shinyApp object
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Combine assessment data (from the branches of the targets work flow) and
#' # open assessment app
#' assessment_data <- combine_assessment_data()
#' open_assessment_app(assessment_data)
#' }
#'
open_assessment_app <- function(assessdata_object, trend_dataframe){

  requireNamespace("ggplot2")

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

  # Define UI for application that draws a histogram
  ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel("Milkys data"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(inputId = "determinand", label = "Parameter", choices = lookup_determinands$determinand, selected = "CD"),
        shiny::uiOutput("stationControls"),
        shiny::uiOutput("matrixControls"),
        shiny::radioButtons(inputId = "plot_points", label = "Points show", choices = c("Annual means", "All data"), selected = "Annual means"),
        shiny::radioButtons(inputId = "logscale", label = "Scale of y axis", choices = c("Log scale", "Linear scale"), selected = "Log scale"),
        shiny::checkboxInput(inputId = "add_trend_text", label = "Show trend text", value = TRUE),
        shiny::textInput("expand_x_txt", "Voluntary: Expand x limits (one/two numbers separated by comma)", value = ""),
        shiny::textInput("expand_y_txt", "Voluntary: Expand y limits (one/two numbers separated by comma)", value = "")
      ),

      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::plotOutput("trendplot"),
        shiny::textInput("plot_title", "Voluntary: custom plot title", value = ""),
        shiny::textInput("plot_subtitle", "Voluntary: custom plot subtitle", value = ""),
        shiny::textInput("plot_ylabel", "Voluntary: custom label for the y axis title", value = "")
        )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    stations_from_determ <- reactive({

      subset(lookup_series, determinand %in% input$determinand)$station |> unique()

    })

    output$stationControls <- shiny::renderUI({
      # browser()
      station_values <- stations_from_determ()
      shiny::selectInput(
        inputId = "station",
        label = "Station",
        choices = station_values, selected = "30B Oslo City area (4684)")
    })

    series_determ_station <- reactive({

      selected_series <- subset(lookup_series, determinand %in% input$determinand & station %in% input$station)

      selected_series

    })

    output$matrixControls <- shiny::renderUI({
      series <- series_determ_station()
      # browser()
      matrix_values <- unique(series$matrix)
      selectInput(inputId = "matrix", label = "Matrix",
                  choices = matrix_values)
    })

    output$trendplot <- shiny::renderPlot({

      selected_matrix <- input$matrix

      selected_series <- subset(
        series_determ_station(),
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

      if (input$expand_x_txt != ""){
        expand_x <- as.numeric(strsplit(input$expand_x_txt, ",")[[1]])
        gg <- gg + ggplot2::expand_limits(x = expand_x)
      }

      if (input$expand_y_txt != ""){
        expand_y <- as.numeric(strsplit(input$expand_y_txt, ",")[[1]])
        gg <- gg + ggplot2::expand_limits(y = expand_y)
      }

      gg

    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
