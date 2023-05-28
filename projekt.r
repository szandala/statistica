WHERE <- "/home/szandala/pwr/statystyka/"
source(paste(WHERE, "funkcje.r", sep=""))
loans_data <- read.csv(paste(WHERE, "loan_sanction_train.csv", sep=""))

count_statistics(loans_data$ApplicantIncome)

draw_histogram(loans_data$ApplicantIncome, xlabel = "Dochód aplikanta")
