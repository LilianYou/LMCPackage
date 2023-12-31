% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quick_plots.R
\name{quick_histogram_plot}
\alias{quick_histogram_plot}
\title{Create a Histogram}
\usage{
quick_histogram_plot(data_frame, col, binwidth = 0.5)
}
\arguments{
\item{data_frame}{A data frame containing the data.}

\item{col}{The name of the numeric column for the x-axis.}

\item{binwidth}{The width of the bins in the histogram (default is 0.5).}
}
\value{
A ggplot2 histogram object.
}
\description{
Generates a histogram for a specified numeric column.
}
\examples{
df <- data.frame(value = rnorm(100))
quick_histogram_plot(df, "value")
}
