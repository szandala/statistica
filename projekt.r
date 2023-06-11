WHERE <- "/home/szandala/pwr/statystyka/"
source(paste(WHERE, "funkcje.r", sep=""))
source(paste(WHERE, "drawings.r", sep=""))
source(paste(WHERE, "outliers.r", sep=""))

loans_data <- read.csv(paste(WHERE, "loan_sanction_train.csv", sep=""), colClasses = c(
    "character","factor","factor","factor","factor",
    "factor","integer","integer",
    "integer","integer","factor",
    "factor","factor"))
loans_data <- read.csv(paste(WHERE, "loan_sanction_train.csv", sep=""))

count_statistics(loans_data$ApplicantIncome)

draw_histogram(loans_data$ApplicantIncome, xlabel = "Dochód aplikanta")
#draw_linear(loans_data$ApplicantIncome, xlabel = "Dochód aplikanta")

#draw_bar()

draw_box(loans_data$ApplicantIncome, xlabel = "Dochód aplikanta", ylabel="Dochód Aplikanta")
draw_density(loans_data$ApplicantIncome, xlabel = "Dochód aplikanta")

get_outliers(loans_data$ApplicantIncome)
table(loans_data$Property_Area)
loans_data$Property_Area = as.factor(loans_data$Property_Area)
draw_pie(table(loans_data$Property_Area))
summary(loans_data)
