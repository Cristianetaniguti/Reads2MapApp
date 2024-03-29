% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_time.R
\name{workflow_times}
\alias{workflow_times}
\title{Measuring cromwell tasks time}
\usage{
workflow_times(log.file, interactive = FALSE)
}
\arguments{
\item{log.file}{character with log file name in cromwell-workflow-logs directory after
running WDL workflow}

\item{interactive}{plots an plotly-type graphic with possibility to see other values in an interactive way}
}
\value{
ggplot boxplot with time spent by each task
}
\description{
Measuring cromwell tasks time
}
