WHERE <- "/home/szandala/pwr/statystyka/"
source(paste(WHERE, "funkcje.r", sep=""))
source(paste(WHERE, "drawings.r", sep=""))
source(paste(WHERE, "outliers.r", sep=""))
source(paste(WHERE, "matrix.r", sep=""))

loans_data <- read.csv(paste(WHERE, "loan_sanction_train.csv", sep=""))
#                       , colClasses = c(
#    "character","factor","factor","factor","factor",
#    "factor","integer","integer",
#    "integer","integer","factor",
#    "factor","factor"))
loans_data <- read.csv(paste(WHERE, "loan_sanction_train.csv", sep=""))

count_statistics(loans_data$ApplicantIncome)
correlation <- cor(loans_data$CoapplicantIncome, loans_data$ApplicantIncome)
print(correlation)
#draw_histogram(loans_data$ApplicantIncome, xlabel = "Dochód aplikanta")
#draw_linear(loans_data$ApplicantIncome, xlabel = "Dochód aplikanta")

#draw_bar()

draw_box2(loans_data$ApplicantIncome, loans_data$CoapplicantIncome, xlabel = "Dochód aplikanta", ylabel="")
#draw_density(loans_data$ApplicantIncome, xlabel = "Dochód aplikanta")

get_outliers(loans_data$ApplicantIncome)
table(loans_data$Property_Area)
loans_data$Gender = as.factor(loans_data$Gender)
loans_data$Married = as.factor(loans_data$Married)
loans_data$Education = as.factor(loans_data$Education)
loans_data$Self_Employed = as.factor(loans_data$Self_Employed)
loans_data$Credit_History = as.factor(loans_data$Credit_History)
loans_data$Property_Area = as.factor(loans_data$Property_Area)
loans_data$Loan_Status = as.factor(loans_data$Loan_Status)
loans_data$Dependents = as.factor(loans_data$Dependents)

summary(loans_data)
#draw_pie(table(loans_data$Property_Area))
#print(loans_data$Property_Area)
par(mfrow = c(2, 2)) 
par(mar = c(2, 2, 1, 1))  
par(oma = c(0.3, 0.3, 0.3, 0.3))

pie(table(loans_data$Property_Area),  main = "Lokalizacje inwestycji")
pie(table(loans_data$Gender),  main = "Płeć aplikantów")
pie(table(loans_data$Married),  main = "W związku")
pie(table(loans_data$Dependents),  main = "Osoby na utrzymaniu")
library(moments)
skew <- skewness(loans_data$ApplicantIncome)
print(skew)

#describe_matrix("ApplicantIncome", "CoapplicantIncome", "LoanAmount", loans_data)

# numeryczn przedział
dane_numeryczne <- loans_data$ApplicantIncome
wynik_testu <- t.test(dane_numeryczne, na.rm = TRUE,  conf.level = 0.90)
przedzial_ufnosci <- wynik_testu$conf.int
print(przedzial_ufnosci)

# jakościowy przedział
dane_jakosciowe <- loans_data$Loan_Status
sukcesy <- sum(dane_jakosciowe == "Y")  # Liczba sukcesów
proba <- length(dane_jakosciowe)  # Liczba próbek
wynik_testu <- binom.test(sukcesy, proba, conf.level = 0.99)
przedzial_ufnosci <- wynik_testu$conf.int
print(przedzial_ufnosci)

####################################
# Hipoteza 1: Porównanie średniego dochodu aplikanta dla osób z wykształceniem magisterskim i nieposiadających wykształcenia magisterskiego
graduate_income <- loans_data[loans_data$Education == "Graduate", "ApplicantIncome"]
non_graduate_income <- loans_data[loans_data$Education == "Not Graduate", "ApplicantIncome"]

t.test(graduate_income, non_graduate_income)

print("######################################")
# Hipoteza 2: Sprawdzenie zależności między dochodem aplikanta a kwotą kredytu
income <- loans_data$ApplicantIncome
coapplicant_income <- loans_data$CoapplicantIncome

cor.test(income, coapplicant_income, method = "pearson")
cor.test(income, coapplicant_income, method = "spearman")
cor.test(income, coapplicant_income, method = "kendall")

############################

# Usunięcie kolumny z identyfikatorem
data <- loans_data[, -1]

# Usunięcie wierszy z brakującymi danymi
data <- na.omit(data)

# Wyodrębnienie zmiennych numerycznych do macierzy
numeric_data <- as.matrix(data[, sapply(data, is.numeric)])

# Standaryzacja zmiennych
scaled_data <- scale(numeric_data)

# Obliczenie macierzy kowariancji
cov_matrix <- cov(scaled_data)

# Obliczenie składowych głównych (PCA)
pca <- prcomp(scaled_data)

# Wyświetlenie wyników
print(pca)


########################
