#' Quick Bar Plot
#'
#' Generates a bar plot for a given column of a data frame using ggplot2.
#'
#' @param data_frame A data frame containing the data for plotting.
#' @param col The name of the column to be plotted.
#' @param base_size Base font size for the plot (default is 12).
#' @param text_size Text font size for the plot (default is 12).
#' @return A ggplot2 bar plot object.
#' @importFrom stats glm binomial coef confint gaussian p.adjust predict rbinom sd wilcox.test as.formula
#' @importFrom ggplot2 aes labs position_dodge theme_minimal
#' @importFrom data.table setDT
#' @importFrom rlang sym
#' @importFrom purrr map map_lgl
#' @importFrom tibble tibble
#' @examples
#' df <- data.frame(category = sample(c("A", "B", "C"), 100, replace = TRUE))
#' quick_bar_plot(df, "category")
quick_bar_plot <- function(data_frame, col, base_size = 12, text_size =12){
  ggplot2::ggplot(data = data_frame) +
    ggplot2::geom_bar(mapping = ggplot2::aes(x = !!rlang::sym(col)))+
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))
}


#' Advanced Bar Plot with Statistics
#'
#' Creates an advanced bar plot with mean and standard error for specified categories
#' using ggplot2.
#'
#' @param data_frame A data frame containing the data for plotting.
#' @param col The name of the column for calculating statistics.
#' @param category1 The primary category for grouping data.
#' @param category2 The secondary category for grouping data (default is "Memory").
#' @param title The title for the plot (default is the value of category1).
#' @return A ggplot2 bar plot object with error bars.
#' @import ggplot2
#' @examples
#' df <- data.table(value = rnorm(100), dx = sample(c("Group1", "Group2"), 100, replace = TRUE),
#' Memory = sample(0:1, 100, replace = TRUE))
#' quick_bar_plot2(df, "value", "dx", "Memory")
quick_bar_plot2 <- function(data_frame, col, category1, category2 = "Memory", title = category1){
  setDT(data_frame)

  # Dynamic column names: compute mean and standard error
  col_stats <- data_frame[, .(Mean_col = mean(get(col), na.rm = TRUE),
                              SE = sd(get(col), na.rm = TRUE)/sqrt(.N)),
                          by = c(category1, category2)]

  col_stats[, (category2) := factor(get(category2), levels = c(1, 0), labels = c("Impaired", "Not Impaired"))]

  # Create the plot
  # print(class(col_stats[[category2]]))
  # print(levels(col_stats[[category2]]))
  ggplot2::ggplot(col_stats, ggplot2::aes(x = category1, y = "Mean_col", fill = category2)) +
    ggplot2::geom_bar(stat = "identity", position = position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Mean_col - SE, ymax = Mean_col + SE),
                  position = position_dodge(0.9), width = 0.25) +
    ggplot2::scale_fill_manual(name = category2, values = c("Impaired" = "#F8766D", "Not Impaired" = "#00BA38")) +
    # scale_fill_manual(name = category2, values = colors)
    ggplot2::labs(title = paste("Mean", gsub("_", " ", col), "by", title, "and", category2), # \n
         x = " ",
         y = paste("Mean", col)) +
    theme_minimal()
  # print(plot)
}


#' Quick Count Plot
#'
#' Generates a frequency polygon plot for specified column and category using ggplot2.
#'
#' @param data_frame A data frame containing the data for plotting.
#' @param col The column name for the x-axis.
#' @param category The column name for the color grouping.
#' @param legend_name The name to be used for the legend (default is the value of category).
#' @param title The title of the plot (default is " ").
#' @param binwidth The width of the bins in the frequency polygon (default is 1).
#' @return A ggplot2 frequency polygon plot object.
#' @import ggplot2
#' @import rlang
#' @examples
#' df <- data.frame(value = rnorm(100), category = sample(c("A", "B"), 100, replace = TRUE))
#' quick_count_plot(df, "value", "category")
quick_count_plot <- function(data_frame, col, category, legend_name = category, title = " ", binwidth = 1){
  ggplot2::ggplot(data = data_frame, mapping = aes(!!sym(col), colour = factor(!!sym(category)))) +
    ggplot2::geom_freqpoly(binwidth = binwidth) +
    ggplot2::labs(colour = legend_name, title = title)
}

#' Create a Density Plot
#'
#' Generates a density plot for a specified numeric column, categorized by another column.
#'
#' @param data_frame A data frame containing the data.
#' @param col The name of the numeric column for the x-axis.
#' @param category The name of the column for categorizing data (used for color coding).
#' @param legend_name The label for the legend (defaults to the name of the category column).
#' @param title The title of the plot.
#' @param alpha The transparency level for the density plots (default is 0.5).
#' @return A ggplot2 density plot object.
#' @import ggplot2
#' @import rlang
#' @examples
#' df <- data.frame(value = rnorm(100), category = sample(c("A", "B"), 100, replace = TRUE))
#' quick_density_plot(df, "value", "category")
quick_density_plot <- function(data_frame, col, category, legend_name = category, title = " ", alpha = 0.5){
  ggplot2::ggplot(data = data_frame, mapping = aes(!!sym(col), colour = factor(!!sym(category)))) +
    ggplot2::geom_density(alpha = alpha) + labs(colour = legend_name, title = title)
}


#' Create a Histogram
#'
#' Generates a histogram for a specified numeric column.
#'
#' @param data_frame A data frame containing the data.
#' @param col The name of the numeric column for the x-axis.
#' @param binwidth The width of the bins in the histogram (default is 0.5).
#' @return A ggplot2 histogram object.
#' @import ggplot2
#' @import rlang
#' @examples
#' df <- data.frame(value = rnorm(100))
#' quick_histogram_plot(df, "value")
quick_histogram_plot <- function(data_frame, col, binwidth = 0.5){
  ggplot2::ggplot(data = data_frame) +
    ggplot2::geom_histogram(mapping = aes(x = !!sym(col)), binwidth = binwidth)
}

#' Create a Scatter Plot
#'
#' Generates a scatter plot comparing two numeric columns.
#'
#' @param data_frame A data frame containing the data.
#' @param x The name of the column for the x-axis.
#' @param y The name of the column for the y-axis.
#' @param title The title of the plot (defaults to the name of the x-axis column).
#' @return A ggplot2 scatter plot object.
#' @import ggplot2
#' @import rlang
#' @examples
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' quick_scatter_plot(df, "x", "y")
quick_scatter_plot <- function(data_frame, x, y, title = x){
  ggplot2::ggplot(data = data_frame, mapping = ggplot2::aes(x = !!sym(x), y = !!sym(y))) +
    ggplot2::geom_point()+ggplot2::ylab(title)+ ggplot2::labs(title = title)
}

#' Create a Box Plot
#'
#' Generates a box plot for comparing distributions across different categories.
#'
#' @param data_frame A data frame containing the data.
#' @param x The name of the categorical column for the x-axis.
#' @param y The name of the numeric column for the y-axis.
#' @param title The title of the plot (defaults to the name of the x-axis column).
#' @param cut_width The width of the grouping for the box plot (default is 0.1).
#' @return A ggplot2 box plot object.
#' @import ggplot2
#' @import rlang
#' @examples
#' df <- data.frame(category = sample(c("A", "B", "C"), 100, replace = TRUE), value = rnorm(100))
#' quick_box_plot(df, "category", "value")
quick_box_plot <- function(data_frame, x, y, title = x){
  ggplot2::ggplot(data = data_frame, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = title)
}

