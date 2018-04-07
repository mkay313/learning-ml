# data: https://archive.ics.uci.edu/ml/datasets/adult

library(tidyverse)
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
names(adult)[names(adult) == "country"] <- "region"

# some workspace cleanup
rm(c_n_am, c_other, c_s_am, c_asia, c_europe, cont, count, emp_end_st, emp_entry_st, i, j, mar_end_st, mar_entry_st)

# turn '?' into NA values and remove ? from factors
adult[adult == '?'] <- NA
print(summary(adult))
adult$type_employer <- factor(as.character(adult$type_employer))
adult$occupation <- factor(as.character(adult$occupation))
print(summary(adult))

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'), legend = FALSE)

# there are 1843 NA occupations (sum(is.na(adult$occupation))) and  1863 NA employer types
# let's see if these coincide
v <- apply(adult, MARGIN = 1, function(x) sum(is.na(x)))
print(length(v[v==2]))
# this means 1863 of them coincide
# let's only filter these out for a moment and see if they have something in common
adult_na <- adult %>%
  filter(is.na(type_employer)) %>%
  filter(is.na(occupation))

ggplot(data = adult_na, aes(age)) + 
  geom_bar(aes(fill = income), alpha = 0.5) + 
  theme_bw()
# by the look of things, the majority of NAs is either young (<25), or retirement age
# how many of them exactly are in the higher income group?

income_na <- adult_na %>%
  group_by(income) %>%
  count()
income_all <- adult %>%
  group_by(income) %>%
  count()
print(income_na$n[2]/income_all$n[2])

# only >2.5% of all higher income folk fall into the NAs so it seems safe, and NAs are ~6% of all folk so let's drop them
adult <- na.omit(adult)

# ages colored by income again
ages_with_income <- ggplot(data = adult, aes(age))
ages_with_income +
  geom_histogram(aes(fill = factor(income)), alpha = 0.5, bins = 35, color = "black") +
  theme_bw()

regions_with_income <- ggplot(data = adult, aes(region))
regions_with_income +
  geom_bar(aes(fill=factor(income)), alpha = 0.5, color = "black") +
  coord_flip() +
  theme_bw()
