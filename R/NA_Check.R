#' Replace Empty Strings with NA in Character Columns of a Data Table
#'
#' @param dt A data table.
#'
#' @return A data table with empty strings in character columns replaced by NA.
#'
#' @examples
#' dt <- data.table(a = c("apple", "", "banana"), b = c("", "orange", "grape"))
#' dt <- replace_empty_with_na(dt)
#' print(dt)
replace_empty_with_na <- function(dt) {
  if (!inherits(dt, "data.table")) {
    stop("Input must be a data table.")
  }

  char_col_indices <- which(map_lgl(dt, is.character))
  empty_col_logical <- dt[, map_lgl(.SD, ~ any(.x == "")), .SDcol = char_col_indices]
  # Map back to original indices
  empty_col_idx <- char_col_indices[empty_col_logical]
  length_empty_col_idx <- sum(empty_col_idx, na.rm = TRUE)
  if (length_empty_col_idx > 0){
    dt[, (empty_col_idx) := map(.SD, ~ ifelse(.x == "", NA, .x)), .SDcols = empty_col_idx]
  }
  return(dt)
}

#' Identify Columns with NA and Count Them
#'
#' @param dt A data table.
#'
#' @return A list with names of columns containing NA and the count of such columns.
#'
#' @examples
#' dt <- data.table(a = c("apple", "", "banana"), b = c("", "orange", "grape"))
#' dt <- replace_empty_with_na(dt)
#' na_info <- identify_na_columns(dt)
#' print(na_info)

identify_na_columns <- function(dt) {
  if (!inherits(dt, "data.table")) {
    stop("Input must be a data table.")
  }

  na_columns <- which(sapply(dt, function(x) any(is.na(x))))
  na_count <- length(na_columns)
  na_column_names <- names(dt)[na_columns]

  return(list(columns_with_na = na_column_names, na_count = na_count))
}

