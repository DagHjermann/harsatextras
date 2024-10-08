% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/open_assessment_app.R
\name{open_assessment_app}
\alias{open_assessment_app}
\title{Test shiny}
\usage{
open_assessment_app(assessdata_object)
}
\arguments{
\item{assessdata_object}{An assessment data object, i.e. the output of \code{get_assessment_data} or \code{combine_assessment_data}.}
}
\value{
shinyApp object
}
\description{
Test shiny
}
\examples{

\dontrun{
# Combine assessment data (from the branches of the targets work flow) and
# open assessment app
assessment_data <- combine_assessment_data()
open_assessment_app(assessment_data)
}

}
