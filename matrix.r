describe_matrix <- function(aaa, bbb, ccc, loans_data) {
  
  library(pastecs)  # Do obliczania skośności i kurtozy
  selected_vars <- c(aaa, bbb, ccc)
  data_matrix <- as.matrix(loans_data[, selected_vars])

  stats <- data.frame(
    Minimum = numeric(3),
    Maximum = numeric(3),
    Median = numeric(3),
    SD = numeric(3),
    Variance = numeric(3),
    Cumulative_Sum = numeric(3),
    Quantile_25 = numeric(3),
    Quantile_75 = numeric(3),
    Skewness = numeric(3),
    Kurtosis = numeric(3),
    Unique_Values = numeric(3),
    Zero_Percentage = numeric(3)
  )
  
  for (i in 1:3) {
    print(dim(data_matrix))
    var_values <- data_matrix[, i]
    #print(var_values)
    non_na_values <- var_values[!is.na(var_values)]
    #print(non_na_values)
    stats[i, "Minimum"] <- min(non_na_values)
    stats[i, "Maximum"] <- max(non_na_values)
    stats[i, "Median"] <- median(non_na_values)
    stats[i, "SD"] <- sd(non_na_values)
    stats[i, "Variance"] <- var(non_na_values)
    stats[i, "Cumulative_Sum"] <- sum(non_na_values)
    stats[i, "Quantile_25"] <- quantile(non_na_values, probs = 0.25)
    stats[i, "Quantile_75"] <- quantile(non_na_values, probs = 0.75)
    stats[i, "Skewness"] <- skewness(non_na_values)
    stats[i, "Kurtosis"] <- kurtosis(non_na_values)
    stats[i, "Unique_Values"] <- length(unique(non_na_values))
    stats[i, "Zero_Percentage"] <- sum(var_values == 0, na.rm = TRUE) / sum(!is.na(var_values)) * 100
  }
  print(stats)
}