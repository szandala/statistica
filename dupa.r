##################
# Tabela przestawna dla zmiennych kategorycznych
table_gender <- table(loans_data$Gender, loans_data$Loan_Status)
table_married <- table(loans_data$Married, loans_data$Loan_Status)
table_education <- table(loans_data$Education, loans_data$Loan_Status)
table_self_employed <- table(loans_data$Self_Employed, loans_data$Loan_Status)
table_property_area <- table(loans_data$Property_Area, loans_data$Loan_Status)

# Test chi-kwadrat dla zmiennych kategorycznych
chi_gender <- chisq.test(table_gender)
chi_married <- chisq.test(table_married)
chi_education <- chisq.test(table_education)
chi_self_employed <- chisq.test(table_self_employed)
chi_property_area <- chisq.test(table_property_area)

# Wyświetlenie wyników testu chi-kwadrat
print(chi_gender)
print(chi_married)
print(chi_education)
print(chi_self_employed)
print(chi_property_area)
