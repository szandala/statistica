count_statistics <- function(column) {
  c_mean <- mean(column)
  c_median <- median(column)
  c_min <- min(column)
  c_max <- max(column)
  c_quantiles <- quantile(column,  probs = seq(0, 1, 0.25)) # default: probs = seq(0, 1, 0.25)
  #print(c_quantiles)
  c_mode <- get_domination(column)
  c_variance <- var(column) # populacji, czy próby
  c_sd <- sqrt(c_variance)
  
  df <- data.frame(Name = c("Średnia_Arytmetyczna", 
                            "Wartość_Minimalna","Kwantyl_Dolny",
                            "Mediana",
                            "Kwantyl_Górny", "Wartość_Maxymalna",
                            "Dominanta", "Wariancja", "Odchylenie_Standardowe"),
                   Value = c(c_mean, 
                             c_min, c_quantiles[2], 
                             c_median,
                             c_quantiles[4], c_max,
                             c_mode, c_variance, c_sd)
  )
  return(df)
}

get_domination <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

