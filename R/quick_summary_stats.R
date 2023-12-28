library(data.table)
library(purrr)
#' Quick Summary of Data
#'
#' This function prints a quick summary of specified ROI (Region of Interest) and symptom
#' statistics from the given data table.
#'
#' @param data_table A data table containing the data to be summarized.
#' @param roi The name of the ROI column in the data table.
#' @param symptom The name of the symptom column in the data table.
#' @return Prints summary statistics to the console.
#' @importFrom stats glm binomial coef confint gaussian p.adjust predict rbinom sd wilcox.test as.formula
#' @importFrom ggplot2 aes labs position_dodge theme_minimal
#' @importFrom data.table setDT
#' @importFrom rlang sym
#' @importFrom purrr map map_lgl
#' @importFrom tibble tibble
#' @examples
#' dt <- data.table("hippocampus" = c(1.5, 2.1, 1.8), "memory" = c(0, 1, 0))
#' quick_summary_stats(dt, "hippocampus", "memory")


quick_summary_stats <- function(data_table, roi, symptom) {
  if (!inherits(data_table, "data.table")) {
    stop("Input must be a data table.")
  }

  print(roi)
  print(summary(data_table[[roi]]))

  print(paste(roi, "No", symptom, "Symptom"))
  no_symptom_data <- data_table[data_table[[symptom]] == 0][[roi]]
  print(summary(no_symptom_data))

  print(paste(roi, "Impaired", symptom, "Symptom"))
  impaired_symptom_data <- data_table[data_table[[symptom]] == 1][[roi]]
  print(summary(impaired_symptom_data))
}
