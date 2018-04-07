# data: https://archive.ics.uci.edu/ml/datasets/adult

library(dplyr)
library(Amelia)

adult <- read.csv("adult_sal.csv")
adult <- adult %>%
  select(-X)

print(str(adult))
print(summary(adult))

# observations:
#    - mostly privately employed
#    - ~1/2 either high school graduates or with some (unfinished) college education
#    - ~45% married, ~1/3 never married
#    - the majority is white, male, American
#    - on average they work 40hrs per week (single job, full-time)

ChangeData <- function(this_vector, entry_status, end_status) {
  this_vector[this_vector == entry_status] <- end_status
  return(this_vector)
}

# employer type cleanup
print(table(adult$type_employer))
adult$type_employer <- as.character(adult$type_employer)
emp_entry_st <- c("Never-worked", "Without-pay", "Local-gov", "State-gov", "Self-emp-inc", "Self-emp-not-inc")
emp_end_st <- c("Unemployed", "Unemployed", "SL-gov", "SL-gov", "Self-emp", "Self-emp")
for (i in 1:length(emp_entry_st)) {
  adult$type_employer <- ChangeData(adult$type_employer, emp_entry_st[i], emp_end_st[i])
}
adult$type_employer <- factor(adult$type_employer)
print(table(adult$type_employer))

# marital status cleanup
print(table(adult$marital))
adult$marital <- as.character(adult$marital)
mar_entry_st <- c("Divorced", "Married-AF-spouse", "Married-civ-spouse", "Married-spouse-absent", "Divorced", "Separated", "Widowed")
mar_end_st <- c("Not-Married", "Married", "Married", "Married", "Not-Married", "Not-Married", "Not-Married")
for (i in 1:length(mar_entry_st)) {
  adult$marital <- ChangeData(adult$marital, mar_entry_st[i], mar_end_st[i])
}
adult$marital <- factor(adult$marital)
print(table(adult$marital))

# countries cleanup
print(table(adult$country))
adult$country <- as.character(adult$country)
c_europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands", "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland", "Yugoslavia")
c_asia <- c("Cambodia", "China", "India", "Iran", "Japan", "Laos", "Philippines", "Taiwan", "Thailand", "Vietnam")
c_n_am <- c("Canada", "Outlying-US(Guam-USVI-etc)", "United-States")
c_s_am <- c("Columbia", "Cuba", "Dominican-Republic", "Ecuador", "El-Salvador", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Peru", "Puerto-Rico", "Trinadad&Tobago")
c_other <- c("?", "Hong", "South")
for (i in 1:5) {
  cont <- c("Europe", "Asia", "North-America", "Latin&South-America", "Other")
  count <- list(c_europe, c_asia, c_n_am, c_s_am, c_other)
  for (j in 1:length(count[[i]])) {
    adult$country <- ChangeData(adult$country, count[[i]][j], cont[i])
  }
}
adult$country <- factor(adult$country)
print(table(adult$country))

# some workspace cleanup
rm(c_n_am, c_other, c_s_am, c_asia, c_europe, cont, count, emp_end_st, emp_entry_st, i, j, mar_end_st, mar_entry_st)

