
#' Calculate Power for Statistical Tests
#'
#' This function calculates the statistical power for different types of tests
#' (Wilcoxon, Logistic Regression, Linear Regression) based on simulation.
#'
#' @param data_frame A data frame containing the data for the power calculation.
#' @param group_column The name of the column that divides the data into groups.
#' @param variable_column The name of the column for which to calculate the power.
#' @param stats_method A string specifying the type of statistical test;
#'        can be "wilcox" for Wilcoxon test, "logistic" for Logistic Regression,
#'        or "regression" for Linear Regression.
#' @param age_column The name of the age column, with a default value of "Age_visit".
#' @return The calculated power as a numeric value.
#' @importFrom stats glm binomial coef confint gaussian p.adjust predict rbinom sd wilcox.test as.formula
#' @importFrom ggplot2 aes labs position_dodge theme_minimal
#' @importFrom data.table setDT
#' @importFrom rlang sym
#' @importFrom purrr map map_lgl
#' @importFrom tibble tibble
#' @examples
#' df <- data.frame(group = sample(0:1, 100, replace = TRUE),
#'                  value = rnorm(100),
#'                  age = rnorm(100, mean = 50, sd = 10), Sex = sample(0:1, 100, replace = TRUE))
#' calculate_power(df, "group", "value", "wilcox")
#' calculate_power(df, "group", "value", "logistic", age_column = "age")
#' calculate_power(df, "group", "value", "regression", age_column = "age")
calculate_power <- function(data_frame, group_column, variable_column, stats_method, age_column = "Age_visit"){
  set.seed(123)
  # For Wilcoxon test
  if(stats_method == "wilcox"){
    nsim <- 1000
    alpha <- 0.05
    power_count <- 0
    # Ensure the variable column is numeric
    if(!is.numeric(data_frame[[variable_column]])) {
      stop(paste(variable_column, "must be numeric"))
    }
    for (i in 1:nsim) {
      # Resample with replacement from each group
      group1_sample <- data_frame[data_frame[[group_column]] == 1, ][[variable_column]]
      group2_sample <- data_frame[data_frame[[group_column]] == 0, ][[variable_column]]
      group1 <- sample(group1_sample, size = length(group1_sample), replace = TRUE)
      group2 <- sample(group2_sample, size = length(group2_sample), replace = TRUE)

      # Perform the Wilcox test
      test_result_sim <- stats::wilcox.test(group1, group2)

      # Count significant outcomes
      if (test_result_sim$p.value < alpha) {
        power_count <- power_count + 1
      }
    }

    # Calculate power
    return(power_count / nsim)
  }

  # For Logistic regression
  if(stats_method == "logistic"){
    num_simulations <- 1000
    alpha <- 0.05
    significant_count <- 0
    n <- nrow(data_frame)

    # Scale the predictor and age

    data_frame[[variable_column]] <- as.numeric(scale(data_frame[[variable_column]]))
    data_frame[[age_column]] <- as.numeric(scale(data_frame[[age_column]]))
    original_fit <- glm(formula = as.formula(paste(group_column, "~", variable_column, "+", age_column,"+ Sex")), data = data_frame, family = stats::binomial)

    for(i in 1:num_simulations) {
      # Simulate predictor variables based on their empirical distribution
      predictor_sim <- sample(data_frame[[variable_column]], size = n, replace = TRUE)
      age_sim <- sample(data_frame[[age_column]], size = n, replace = TRUE)
      sex_sim <- sample(data_frame$Sex, size = n, replace = TRUE)  # Assuming 'sex' is a covariate
      # Simulate predictor variables based on their empirical distribution
      #simulated_data <- data.frame(variable_column = predictor_sim, age_column = age_sim)
      # simulated_data[[variable_column]] <- sample(simulated_data[[variable_column]], size = n, replace = TRUE)
      # simulated_data[[age_column]] <- sample(simulated_data[[age_column]], size = n, replace = TRUE)
      # simulated_data$Sex <- sample(simulated_data$Sex, size = n, replace = TRUE)
      # # Fit the original model to the data


      # Calculate probabilities for the simulated response
      simulated_data <- data.frame(variable_column = predictor_sim, age_column = age_sim, Sex = sex_sim)
      colnames(simulated_data) <- c(variable_column, age_column, "Sex")
      log_odds <- predict(original_fit, newdata = simulated_data, type = "link")
      prob <- 1 / (1 + exp(-log_odds))
      simulated_response <- rbinom(n, size = 1, prob = prob)

      # Fit the logistic model to the simulated data
      simulated_data[[group_column]] <- simulated_response
      fit_sim <- glm(formula = as.formula(paste(group_column, "~", variable_column, "+", age_column,"+ Sex")),
                     data = simulated_data, family = binomial)
      # Check if the effect of the predictor is significant
      if(coef(summary(fit_sim))[variable_column, "Pr(>|z|)"] < alpha) {
        significant_count <- significant_count + 1
      }
    }

    # Calculate and return the power
    return(significant_count / num_simulations)
  }

  # For Linear regression
  if(stats_method == "regression"){
    num_simulations <- 1000
    alpha <- 0.05
    significant_count <- 0
    n <- nrow(data_frame)

    # Scale the predictor and age
    data_frame[[variable_column]] <- as.numeric(scale(data_frame[[variable_column]]))
    data_frame[[age_column]] <-  as.numeric(scale(data_frame[[age_column]]))

    for(i in 1:num_simulations) {
      # Resample with replacement from the original data
      sampled_data <- data_frame[sample(1:n, size = n, replace = TRUE), ]

      # Fit the regression model
      fit_sim <- glm(formula = as.formula(paste(variable_column, "~", group_column, "+", age_column,"+ Sex")),
                     data = sampled_data, family = gaussian)

      # Check for significance
      if(coef(summary(fit_sim))[[group_column, "Pr(>|t|)"]] < alpha) {
        significant_count <- significant_count + 1
      }
    }

    # Calculate and return the power
    return(significant_count / num_simulations)
  }
}

# For power simulations, you generally simulate the Type I error rate (alpha) for each test as if it were the only test being conducted. The reason for this is that power simulations are designed to estimate how often you would detect an effect if one truly exists, and this is done for each test in isolation.
#
# After conducting your simulations and tests, you would then apply the BH method to the p-values obtained from your actual study to adjust for multiple comparisons. The BH method will give you adjusted p-values or q-values, which you can then use to determine which results are statistically significant at your desired FDR level.

#' Calculate Cliff's Delta (Effect Size) for Wilcoxon Test
#'
#' This function calculates Cliff's Delta, an effect size measure for the Wilcoxon test,
#' given two groups in a data table.
#'
#' @param data_table A data table containing the groups to compare.
#' @param group_column The name of the column that divides the data into two groups.
#' @param variable_column The name of the column for which to calculate Cliff's Delta.
#' @return An object containing the estimate of Cliff's Delta and its confidence interval.
#' @importFrom stats glm binomial coef confint gaussian p.adjust predict rbinom sd wilcox.test as.formula
#' @importFrom ggplot2 aes labs position_dodge theme_minimal
#' @importFrom data.table setDT
#' @importFrom rlang sym
#' @importFrom purrr map map_lgl
#' @importFrom tibble tibble
#' @examples
#' dt <- data.table(group = sample(0:1, 100, replace = TRUE),
#'                  value = rnorm(100))
#' calculate_cliffs_delta(dt, "group", "value")
calculate_cliffs_delta <- function(data_table, group_column, variable_column) {
  # Split the data into two groups based on the group column
  variable_column <- gsub("`", "", variable_column)
  group1 <- data_table[data_table[[group_column]] == 1, .SD, .SDcol= variable_column]
  group2 <- data_table[data_table[[group_column]] == 0, .SD, .SDcol= variable_column]

  # # Handle cases where one or both groups are empty
  # if (length(group1) == 0 || length(group2) == 0) {
  #     return(NA)  # Return NA if either group is empty
  # }
  # # Calculate pairwise differences
  # differences <- outer(group1[[variable_column]], group2[[variable_column]], FUN = "-")
  #
  # # Calculate proportions
  # prop_positive <- mean(differences > 0)
  # prop_negative <- mean(differences < 0)
  # # prop_zero <- mean(differences == 0) # Uncomment if you need the proportion of ties
  #
  # # Compute Cliff's Delta
  # cliffs_delta <- prop_positive - prop_negative
  cliffs_delta_result <- effsize::cliff.delta(group1[[variable_column]], group2[[variable_column]])
  # cliffs_delta <- round(cliffs_delta_result$estimate,3)
  # conf_int <- paste0("[", round(cliffs_delta_result$conf.int[1],3), ",",round(cliffs_delta_result$conf.int[2],3), "]")
  return(cliffs_delta_result)
}


#' Perform Statistical Tests on Multiple Columns
#'
#' This function applies specified statistical tests to multiple columns of a data frame
#' and returns the results, including p-values and effect sizes.
#'
#' @param data_frame A data frame containing the data for the tests.
#' @param columns_to_test A vector of column names to be tested.
#' @param reference_column The name of the reference column for comparison in tests.
#' @param stats_method A string specifying the type of test to perform;
#'        can be "wilcox" for Wilcoxon test, "logistic" for Logistic Regression,
#'        or "regression" for Linear Regression.
#' @param Age The name of the age column, with a default value of "Age_visit".
#' @param use_weights A boolean indicating whether to use weights in logistic regression.
#' @return A data frame with test results for each column, including p-values and effect sizes.
#' @importFrom stats glm binomial coef confint gaussian p.adjust predict rbinom sd wilcox.test as.formula
#' @importFrom ggplot2 aes labs position_dodge theme_minimal
#' @importFrom data.table setDT
#' @importFrom rlang sym
#' @importFrom purrr map map_lgl
#' @importFrom tibble tibble
#' @examples
#' df <- data.frame(group = sample(0:1, 100, replace = TRUE),
#'                   counts = sample(0:20, 100, replace = TRUE),
#'                  value1 = rnorm(100),
#'                  value2 = rnorm(100),
#'                  age = rnorm(100, mean = 50, sd = 10),
#'                  Sex = sample(c("Male", "Female"), 100, replace = TRUE))
#' setDT(df)
#' stats_test_function(df, c("value1", "value2"), "group", "wilcox")
#' stats_test_function(df, c("value1", "value2"), "group", "logistic", Age = "age")
#' stats_test_function(df, c("value1", "value2"), "counts", "regression", Age = "age")
stats_test_function <- function(data_frame, columns_to_test, reference_column, stats_method, Age = "Age_visit", use_weights = FALSE) {

  if (stats_method == "wilcox"){
    # Applying Wilcoxon test for each column against the reference column and combining results
    results <- purrr::map_dfr(columns_to_test, function(column_name) {
      # Dynamically create the formula for each column

      column_name <- paste0("`", column_name, "`") # make safe column names
      formula <- as.formula(paste0(column_name, " ~ ", reference_column))
      # print(formula)
      # Perform the Wilcoxon test
      test_result <- wilcox.test(formula, data = data_frame)

      cliffs_delta_result <- calculate_cliffs_delta(data_frame, reference_column, column_name)
      # print(cliffs_delta_result)

      cliffs_delta <- round(cliffs_delta_result$estimate,3)

      conf_int <- paste0("[", round(cliffs_delta_result$conf.int[1],3), ",",round(cliffs_delta_result$conf.int[2],3), "]")

      # Return a data frame with the p-value and the column name (ROI)
      column_name <- gsub("`", "", column_name)
      test_power <- calculate_power(data_frame, reference_column, column_name, stats_method)
      # print(test_power)
      tibble::tibble(ROI = column_name, pval = round(test_result$p.value,3), effect_size_cliffs_delta = cliffs_delta, effect_size_CI = conf_int,
             power = test_power)
    })

  }

  if (stats_method == "logistic"){

    results <- purrr::map_dfr(columns_to_test, function(column_name) {
      # Dynamically create the formula for each column
      formula <- as.formula(paste0(reference_column, " ~ scale(", column_name, ") + scale(", Age, ") + Sex"))

      if(use_weights == TRUE){#print(formula)
        weights <- ifelse(data_frame[[reference_column]] == 1, 1/sum(data_frame[[reference_column]] == 1), 1/sum(data_frame[[reference_column]] == 0))
        # Perform the logistic regression test
        model <- glm(formula, data = data_frame, family = binomial, weights = weights)
        test_results <- summary(model)

      } else{
        # Perform the logistic regression test
        model <- glm(formula, data = data_frame, family = binomial)
        test_results <- summary(model)

      }

      # 95% CI for odds ratio
      confint_log_odds <- confint(model)
      confint_odds_ratio <- round(exp(confint_log_odds),3)

      roi_odds_ratio_ci_l <- confint_odds_ratio[2,1]
      roi_odds_ratio_ci_u <- confint_odds_ratio[2,2]
      roi_odds_ratio_ci <- paste0("[", roi_odds_ratio_ci_l, ",", roi_odds_ratio_ci_u,"]")


      # Return a data frame with the p-value and the column name (ROI)
      column_name <- gsub("`", "", column_name)
      # power
      test_power <- calculate_power(data_frame, reference_column, column_name, stats_method, Age)

      tibble::tibble(ROI = column_name, pval = round(test_results$coefficients[2, 4],3), effect_size_odds_ratio = round(exp(test_results$coefficients[2, 1]),3), effect_size_CI = roi_odds_ratio_ci, power = test_power)

    })
  }

  if (stats_method == "regression"){

    results <- purrr::map_dfr(columns_to_test, function(column_name) {
      # Dynamically create the formula for each column
      column_name <- paste0("`", column_name, "`") # make safe column names
      formula <- as.formula(paste0(reference_column, " ~ scale(", column_name, ") + scale(", Age, ") + Sex"))
      #print(formula)

      # Perform the regression test
      test_results <- summary(glm(formula, data = data_frame, family = gaussian))
      #print(test_results)

      # 95% CI of effect size

      estimate_roi <- test_results$coefficients[2,1]
      se_roi <-test_results$coefficients[2,2]

      # Confidence intervals
      lower_bound_roi <- round(estimate_roi - 1.96 * se_roi,3)
      upper_bound_roi <- round(estimate_roi + 1.96 * se_roi,3)
      roi_beta_ci <- paste0("[", lower_bound_roi, ",",upper_bound_roi,"]")


      # Return a data frame with the p-value and the column name (ROI)
      column_name <- gsub("`", "", column_name)
      # power
      test_power <- calculate_power(data_frame, reference_column, column_name, stats_method, Age)

      tibble(ROI = column_name, tval = round(test_results$coefficients[2, 3], 2), pval = round(test_results$coefficients[2, 4],3), effect_size_beta = round(test_results$coefficients[2, 1], 3), effect_size_CI = roi_beta_ci, power = test_power)
    })
  }
  p_adjusted <- round(p.adjust(results$pval, "BH"), 3)
  results <- cbind(results, p_adjusted)
  return(results)


}
