
draw_histogram <- function(column, title = "", xlabel="", ylabel = "Liczba osób") {
  if (title == "")
    title = xlabel
  hist(column, main = paste("Histogram: ", title), xlab = xlabel, ylab = ylabel)
}

draw_box <- function(column, title = "", xlabel="", ylabel = "Liczba osób") {
  if (title == "")
    title = xlabel
  boxplot(column, main = paste("Wykres pudełkowy: ", title), 
          horizontal = FALSE,
          xlab = xlabel, ylab = ylabel)
  
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