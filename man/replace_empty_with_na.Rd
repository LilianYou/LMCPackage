% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NA_Check.R
\name{replace_empty_with_na}
\alias{replace_empty_with_na}
\title{Replace Empty Strings with NA in Character Columns of a Data Table}
\usage{
replace_empty_with_na(dt)
}
\arguments{
\item{dt}{A data table.}
}
\value{
A data table with empty strings in character columns replaced by NA.
}
\description{
Replace Empty Strings with NA in Character Columns of a Data Table
}
\examples{
dt <- data.table(a = c("apple", "", "banana"), b = c("", "orange", "grape"))
dt <- replace_empty_with_na(dt)
print(dt)
}
