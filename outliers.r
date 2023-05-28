outlierSD <- function(x, nmads = 3) {
  z_scores <- (x - mean(x)) / sd(x)
  outliers <- x[abs(z_scores) > nmads]
  return(outliers)
}
get_outliers <- function(column) {
  sd_outliers <- outlierSD(column, nmads = 2)  # Adjust the number of standard deviations as desired
  
  # Interquartile Range (IQR) Method
  boxplot_stats <- boxplot.stats(column)
  Q1 <- boxplot_stats$stats[2]
  Q3 <- boxplot_stats$stats[4]
  IQR <- Q3 - Q1
  k <- 1.5  # Adjust this value as desired
  iqr_outliers <- column[column < Q1 - k * IQR | column > Q3 + k * IQR]
  
  # Z-Score Method
  threshold <- 2  # Adjust the z-score threshold as desired
  z_scores <- scale(column)
  z_score_outliers <- column[abs(z_scores) > threshold]
  
  # Tukey's Fences Method
  tukey_outliers <- boxplot_stats$out
  
  # Print outliers
  cat("Standard Deviation Method - Outliers:\n", sort(sd_outliers), "\n")
  # Znajduje obserwacje, które znajdują się poza określoną liczbą odchyleń standardowych od średniej.
  cat("IQR Method - Outliers:\n", sort(iqr_outliers), "\n")
  # Metoda zakresu międzykwartylowego (IQR - Interquartile Range Method):
  # Znajduje obserwacje, które są poniżej wartości Q1 - k * IQR lub powyżej wartości Q3 + k * IQR, gdzie k to określona stała.
  cat("Z-Score Method - Outliers:\n", sort(z_score_outliers), "\n")
  # Znajduje obserwacje, które mają z-score większy niż określony próg.
  cat("Tukey's Fences Method - Outliers:\n", sort(tukey_outliers), "\n")
  # Metoda ogrodzeń Tukeya (Tukey's Fences Method):
  # Znajduje obserwacje, które są poniżej wartości Q1 - k * IQR lub powyżej wartości Q3 + k * IQR, gdzie k jest zwykle ustawione na 1,5 lub 3.
  # Podsumowując, metoda IQR i metoda ogrodzeń Tukeya opierają się na obliczeniach IQR, ale różnią się w sposobie traktowania granic. Metoda IQR stosuje granice jako bezwzględne wartości do identyfikacji odstających, podczas gdy metoda ogrodzeń Tukeya używa granic jako punktów orientacyjnych, które mogą sugerować potencjalne obserwacje odstające na wykresie pudełkowym.
}