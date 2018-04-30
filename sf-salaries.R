library(RSQLite)
library(tidyverse)
library(Amelia)
# dataset: https://www.kaggle.com/kaggle/sf-salaries/version/2

con = dbConnect(RSQLite::SQLite(), dbname = "sf-salaries/database.sqlite")
salaries <- dbGetQuery(con, 'select * from Salaries')
dbDisconnect(con)

# some summaries for starters
print(head(salaries))
print(summary(salaries))
print(str(salaries))

# let's remove the Not Provided employees
salaries <- salaries[!salaries$Benefits == "Not Provided", ]

# let's add some missing Benefits data
salaries$Benefits[salaries$Benefits == ""] <- "NA"

# let's see how we can group people
print(salaries %>% group_by(JobTitle) %>% count() %>% arrange(desc(n))) # that's a lot of groups
# best paid jobs, not surprisingly, have Chief in their names
print(salaries %>% group_by(JobTitle) %>% summarise(MeanTotalPayBenefits = mean(TotalPayBenefits)) %>% arrange(desc(MeanTotalPayBenefits)))
print(salaries %>% group_by(Year) %>% count())

# there seems to be a problem in Employee names -- some of them have extra spaces that prevent us grouping them, let's remove them along with dots
salaries$EmployeeName <- gsub("\\.", " ", salaries$EmployeeName)
salaries$EmployeeName <- gsub("  ", " ", salaries$EmployeeName)

print(salaries %>% group_by(EmployeeName) %>% count() %>% arrange(desc(n))) 

# well...there's quite a few Lee's, etc. Maybe let's see if we can differentiate them based on their jobs
print(salaries %>% group_by(JobTitle, EmployeeName, Year) %>% count() %>% arrange(desc(n)))

# there seem to be quite a lot of people who either work 3 jobs or their name is extremely popular within the same occupation ;p
# let's see how many people are in this kind of a situation
salaries_grouped <- salaries %>% group_by(JobTitle, EmployeeName, Year) %>% count() %>% arrange(desc(n))
nrow(salaries_grouped)
nrow(salaries_grouped[salaries_grouped$n > 1, ])

# How about we merge those people into single year records?
salaries_aggregated <- aggregate(TotalPayBenefits ~ JobTitle + EmployeeName + Year, data = salaries, FUN = sum)