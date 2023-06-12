
draw_histogram <- function(column, title = "", xlabel="", ylabel = "Liczba osób") {
  if (title == "")
    title = xlabel
  histogram <- hist(column, main = paste("Histogram: ", title), xlab = xlabel, ylab = ylabel)

  for (i in 1:length(histogram$counts)) {
    text(histogram$mids[i], histogram$counts[i], labels = histogram$counts[i], pos = 1)
  }
}

draw_box <- function(column, title = "", xlabel="", ylabel = "Liczba osób") {
  if (title == "")
    title = xlabel
  boxplot(column, main = paste("Wykres pudełkowy: ", title), 
          horizontal = TRUE,
          xlab = xlabel, ylab = ylabel, outline = FALSE)
  
  stats <- boxplot.stats(column)
  Q1 <- stats$stats[2]
  median <- stats$stats[3]
  Q3 <- stats$stats[4]
  
  # Create legend box
  legend("topright", 
         legend = c(paste("Q1 =", Q1),paste("Median =", median), paste("Q3 =", Q3)),
         border = "black",
         text.col = "black", bty = "l")
}

draw_box2 <- function(column, column2, title = "", xlabel="", ylabel = "Liczba osób") {
  if (title == "")
    title = xlabel
  
  combined_data <- c(column, column2)
  
  # Tworzenie wektorów oznaczających grupy
  group1 <- rep("Dochód Aplikanta", length(column))
  group2 <- rep("Dochód co-Aplikanta", length(column2))
  groups <- c(group1, group2)
  
  boxplot(combined_data ~ groups, main = paste("Wykres pudełkowy: ", title), 
          horizontal = TRUE,
          xlab = xlabel, ylab = ylabel, outline = FALSE)
  


}


draw_density <- function(column, title = "", xlabel="", ylabel = "Liczba osób") {
  if (title == "")
    title = xlabel
  data <- density(column)
  plot(data, main = paste("Gęstość: ", title), xlab = xlabel, ylab = ylabel)
}

draw_bar <- function (columnX, columnY, title = "", xlabel="", ylabel = "Liczba osób") {
  if (title == "")
    title = xlabel
  
  barplot(height=columnY, names.arg = columnX,
          main = paste("Wykres: ", title), xlab = xlabel, ylab = ylabel)
  
}
draw_pie <- function (columnX, labels, title = "") {
  pie(columnX,  main = paste("Wykres: ", title))
  
  
}