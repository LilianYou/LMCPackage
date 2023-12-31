% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quick_plots.R
\name{quick_count_plot}
\alias{quick_count_plot}
\title{Quick Count Plot}
\usage{
quick_count_plot(
  data_frame,
  col,
  category,
  legend_name = category,
  title = " ",
  binwidth = 1
)
}
\arguments{
\item{data_frame}{A data frame containing the data for plotting.}

\item{col}{The column name for the x-axis.}

\item{category}{The column name for the color grouping.}

\item{legend_name}{The name to be used for the legend (default is the value of category).}

\item{title}{The title of the plot (default is " ").}

\item{binwidth}{The width of the bins in the frequency polygon (default is 1).}
}
\value{
A ggplot2 frequency polygon plot object.
}
\description{
Generates a frequency polygon plot for specified column and category using ggplot2.
}
\examples{
df <- data.frame(value = rnorm(100), category = sample(c("A", "B"), 100, replace = TRUE))
quick_count_plot(df, "value", "category")
}
