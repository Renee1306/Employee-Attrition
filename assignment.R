#NYONG CHIN VENN
#TP068452

#Data Import
#read csv file
empdata = read.csv("employee_attrition.csv",header=TRUE)
View(empdata)

#package installation
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("plotrix")

#library loading
library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(magrittr)
library(tidyverse)
library(plotrix)
library(gridExtra)

#Data Preprocessing
#rename column header
newcolnames <- c("EmployeeID", "RecordDate", "BirthDate", "HireDate", "TerminationDate",
                 "Age", "LengthOfService", "CityName", "DepartmentName", "JobTitle",
                 "StoreName", "GenderShort", "GenderFull", "TermReasonDesc", "TermTypeDesc",
                 "StatusYear", "Status", "BusinessUnit")
colnames(empdata) <- newcolnames
View(empdata)


#correct wrong names
empdata=empdata %>%
  mutate(CityName=ifelse(CityName=="New Westminister","New Westminster",CityName))
empdata=empdata %>%
  mutate(TermReasonDesc=ifelse(TermReasonDesc=="Resignaton","Resignation",TermReasonDesc))
empdata=empdata %>%
  mutate(JobTitle=ifelse(JobTitle=="CHief Information Officer","Chief Information Officer",JobTitle))
#changing datatype
empdata$StatusYear <- as.numeric(empdata$StatusYear)
empdata$HireDate <- as.Date(empdata$HireDate, format = "%m/%d/%Y")
empdata$BirthDate <- as.Date(empdata$BirthDate, format = "%m/%d/%Y")
empdata$TerminationDate <- as.Date(empdata$TerminationDate, format = "%m/%d/%Y")
empdata$RecordDate <- as.Date(empdata$RecordDate, format = "%m/%d/%Y %H:%M")
str(empdata)

#Data Cleaning
#check missing value 
sum(is.na(empdata))
colSums(is.na(empdata))
#remove gender short
empdata$GenderShort<-NULL
View(empdata)

#Data Exploration
#structure
str(empdata)
#number of rows
nrow(empdata)
#number of columns
ncol(empdata)
#column names
names(empdata)
#summary
summary(empdata)
#first six row
head(empdata)
#last six row
tail(empdata)
#select random row from dataset
sample_n(empdata,5)
#factor
unique(empdata$CityName)
unique(empdata$DepartmentName)
unique(empdata$BusinessUnit)
unique(empdata$JobTitle)
unique(empdata$StoreName)
unique(empdata$TermReasonDesc)
unique(empdata$TermTypeDesc)
unique(empdata$Status)
unique(empdata$StatusYear)

### Analysis

## Question 1: What is the employee demographics and trend?
## Analysis 1.1 - Number of employee every year with average
active_employees <- empdata %>%
  filter(Status == "ACTIVE")
employee_counts <- active_employees %>%
  group_by(StatusYear) %>%
  summarise(count = n())
ggplot(employee_counts, aes(x = as.factor(StatusYear), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(title = "Number of Employees by Year", x = "Year", y = "Number of Employees") +
  theme_minimal()
average_employees <- empdata %>%
  group_by(StatusYear) %>%
  summarise(count = n()) %>%
  summarise(average_count = mean(count))
cat("Average number of employees per year:", average_employees$average_count, "\n")


## Analysis 1.2 - Number of new hired employee every year with average
first_records <- empdata %>%
  group_by(EmployeeID) %>%
  slice(1)
first_records <- first_records %>%
  mutate(HireYear = year(HireDate))
new_hires_per_year <- first_records %>%
  group_by(HireYear) %>%
  summarise(num_hires = n())
ggplot(new_hires_per_year, aes(x = HireYear, y = num_hires)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  xlab("Year") +
  ylab("Number of New Hires") +
  ggtitle("Number of New Hires per Year")+
  theme_minimal()
average_new_hires <- mean(new_hires_per_year$num_hires)
cat("Average number of new hiring employees per year:", as.integer(average_new_hires))

## Analysis 1.3 - Average Number of Employees in Different Age Groups per Year
active_employees <- empdata %>%
  filter(Status == "ACTIVE")
age_groups <- c("18-25", "26-35", "36-45", "46-55", "56+")
active_employees <- active_employees %>%
  mutate(AgeGroup = cut(Age, breaks = c(18, 26, 36, 46, 56, Inf), labels = age_groups, include.lowest = TRUE))
age_group_counts <- active_employees %>%
  group_by(AgeGroup, StatusYear) %>%
  summarize(count = n())
average_age_group_counts <- age_group_counts %>%
  group_by(AgeGroup) %>%
  summarize(average_count = as.integer(mean(count)))
ggplot(average_age_group_counts, aes(x = AgeGroup, y = average_count, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = average_count), vjust = -0.5, color = "black") +  
  labs(title = "Average Number of Employees in Different Age Groups per Year",
       x = "Age Group", y = "Average Number of Employees") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

## Analysis 1.4- Overall average age of employee
average_age_by_year <- empdata %>%
  group_by(StatusYear) %>%
  summarize(AverageAge = mean(Age, na.rm = TRUE))
overall_average_age <- mean(average_age_by_year$AverageAge, na.rm = TRUE)
cat("Overall average age of employees:", as.integer(overall_average_age), "\n")

#Analysis 1.5 - Overall distribution of employees across different cities
year_2006_to_2015_data <- empdata %>%
  filter(StatusYear >= 2006 & StatusYear <= 2015 & Status =="ACTIVE")
cityname_counts <- year_2006_to_2015_data %>%
  group_by(CityName, StatusYear) %>%
  summarise(total_employees = n())
average_cityname_counts <- cityname_counts %>%
  group_by(CityName) %>%
  summarise(average_employees = mean(total_employees))
ggplot(average_cityname_counts, aes(x = CityName, y = average_employees, fill = CityName)) +
  geom_bar(stat = "identity") +
  xlab("City Name") +
  ylab("Average Number of Employees") +
  ggtitle("Average Number of Employees by City Name (2006-2015)") +
  scale_fill_hue() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Analysis 1.6 – Gender distribution of employee every year
active_data <- empdata %>%
  filter(Status =="ACTIVE")
gender_counts <- active_data %>%
  group_by(StatusYear, GenderFull) %>%
  summarise(EmployeeCount = n())
ggplot(gender_counts, aes(x = GenderFull, y = EmployeeCount, fill = GenderFull)) +
  geom_bar(stat = "identity") +
  xlab("Gender") +
  ylab("Employee Count") +
  ggtitle("Gender Distribution of Employees") +
  facet_wrap(~ StatusYear, ncol = 3) +
  scale_fill_manual(values = c("pink", "skyblue")) +
  theme_minimal()

## Analysis 1.7 – Overall distribution of employee for different business unit
year_2006_to_2015_data <- empdata %>%
  filter(Status == "ACTIVE")
bu_counts <- year_2006_to_2015_data %>%
  group_by(BusinessUnit, StatusYear) %>%
  summarise(total_employees = n())
average_bu_counts <- bu_counts %>%
  group_by(BusinessUnit) %>%
  summarise(average_employees = mean(total_employees))
ggplot(average_bu_counts, aes(x = BusinessUnit, y = average_employees, fill = BusinessUnit)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  xlab("Business Unit") +
  ylab("Number of Employees") +
  ggtitle("Distribution of Employees by Business Unit (2006-2015)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label = average_employees), vjust = -0.5, color = "black", size = 3)
total_employees <- sum(average_bu_counts$average_employees)
average_bu_counts$employee_percentage <- round((average_bu_counts$average_employees / total_employees) * 100, 1)
print(average_bu_counts[, c("BusinessUnit", "employee_percentage")])

## Analysis 1.8- Overall distribution of employee for different department
year_2006_to_2015_data <- empdata %>%
  filter(StatusYear >= 2006 & StatusYear <= 2015 & Status =="ACTIVE")
department_counts <- year_2006_to_2015_data %>%
  group_by(DepartmentName, StatusYear) %>%
  summarise(total_employees = n())
average_department_counts <- department_counts %>%
  group_by(DepartmentName) %>%
  summarise(average_employees = mean(total_employees))
ggplot(average_department_counts, aes(x = DepartmentName, y = average_employees, fill = DepartmentName)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(average_employees), vjust = -0.5), size = 3) + 
  xlab("Department Name") +
  ylab("Average Number of Employees") +
  ggtitle("Average Number of Employees by Department (2006-2015)") +
  scale_fill_hue() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Analysis 1.9- Overall distribution of employee for different job title
year_2006_to_2015_data <- empdata %>%
  filter(StatusYear >= 2006 & StatusYear <= 2015 & Status =="ACTIVE")
jobtitle_counts <- year_2006_to_2015_data %>%
  group_by(JobTitle, StatusYear) %>%
  summarise(total_employees = n())
average_jobtitle_counts <- jobtitle_counts %>%
  group_by(JobTitle) %>%
  summarise(average_employees = mean(total_employees))
ggplot(average_jobtitle_counts, aes(x = JobTitle, y = average_employees)) +
  geom_bar(stat = "identity",fill = "steelblue") +
  geom_text(aes(label = round(average_employees), vjust = -0.5), size = 3) + 
  xlab("Job Title") +
  ylab("Average Number of Employees") +
  ggtitle("Average Number of Employees by Job Title (2006-2015)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Analysis 1.10- Overall distribution of employee across different store
year_2006_to_2015_data <- empdata %>%
  filter(StatusYear >= 2006 & StatusYear <= 2015 & Status =="ACTIVE")
storename_counts <- year_2006_to_2015_data %>%
  group_by(StoreName, StatusYear) %>%
  summarise(total_employees = n())
average_storename_counts <- storename_counts %>%
  group_by(StoreName) %>%
  summarise(average_employees = mean(total_employees))
average_storename_counts$StoreName <- factor(average_storename_counts$StoreName)
ggplot(average_storename_counts, aes(x = StoreName, y = average_employees, fill = StoreName)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(average_employees), vjust = -0.5), size = 3) + 
  xlab("Store Name") +
  ylab("Average Number of Employees") +
  ggtitle("Average Number of Employees by Store Name (2006-2015)") +
  scale_fill_hue() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Question 2 - What is the main factor that leads to employee attrition, and 
#               what are their characteristics?

## Analysis 2.1– Number of employees terminated each year
terminated_data <- empdata %>%
  filter(Status == "TERMINATED")
termination_counts <- terminated_data %>%
  group_by(StatusYear) %>%
  summarise(termination_count = n())
ggplot(termination_counts, aes(x = StatusYear, y = termination_count)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = termination_count), vjust = -0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("Year") +
  ylab("TNumber of Employee") +
  ggtitle("Number of Employees Terminated Each Year") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(termination_counts$StatusYear), 
                                  max(termination_counts$StatusYear)))

## Analysis 2.2 - Termination type and reason of employee over years
terminated_employees <- empdata %>%
  filter(Status == "TERMINATED")
termination_analysis <- terminated_employees %>%
  group_by(TermTypeDesc, TermReasonDesc) %>%
  count() %>%
  arrange(TermTypeDesc, desc(n))
ggplot(termination_analysis, aes(x = TermTypeDesc, y = n, fill = TermReasonDesc)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +  
  labs(title = "Termination Type and Termination Reason",
       x = "Termination Type",
       y = "Number of employee",
       fill = "Termination Description") +
  theme_minimal() +
  theme(legend.position = "right")

## Analysis 2.3 - Average age for voluntary retirement of employees
retired_employees <- empdata %>%
  filter(TermTypeDesc == "Voluntary" & TermReasonDesc=="Retirement")
average_age <- retired_employees %>%
  summarise(average_age = round(mean(Age),2))
print(average_age)

## Analysis 2.4 - Average length of service for voluntary retirement of employees
retired_employees <- empdata %>%
  filter(TermTypeDesc == "Voluntary" & TermReasonDesc=="Retirement")
average_length_of_service <- retired_employees %>%
  summarise(average_length_of_service = round(mean(LengthOfService), 2))
print(average_length_of_service)

## Analysis 2.5 - Terminations of employee by Age Group with Termination Reason
terminated_employees <- empdata %>%
  filter(Status == "TERMINATED")
terminated_employees <- terminated_employees %>%
  mutate(age_groups = cut(Age, breaks = c(0, 30, 40, 50, 60, Inf), 
                          labels = c("<=30", "31-40", "41-50", "51-60", "60+")))
termination_counts <- terminated_employees %>%
  group_by(age_groups, TermReasonDesc) %>%
  summarise(termination_count = n())
ggplot(termination_counts, aes(x = age_groups, y = termination_count, fill = TermReasonDesc)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = termination_count), position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  labs(title = "Age Group with Termination Reason",
       x = "Age Group",
       y = "Termination Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## Analysis 2.6 - Terminations of employee by City Name with Termination Reason
filtered_data <- empdata %>%
  filter(Status == "TERMINATED")  # Include only terminated employees
city_counts <- filtered_data %>%
  group_by(CityName, TermReasonDesc) %>%
  count()
ggplot(city_counts, aes(x = CityName, y = n, fill = TermReasonDesc)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), 
            color = "black", size = 2) +
  labs(title = "City Name with Termination Reason",
       x = "City Name",
       y = "Number of Employee") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Analysis 2.7 – Terminations of employee by Gender with Termination Reason
terminated_employees <- empdata %>%
  filter(Status == "TERMINATED")
termination_counts <- terminated_employees %>%
  group_by(GenderFull, TermReasonDesc) %>%
  summarize(TotalTerminations = n())
ggplot(termination_counts, aes(x = GenderFull, y = TotalTerminations, fill = TermReasonDesc)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = TotalTerminations), position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  labs(title = "Gender with Termination Reason",
       x = "Gender",
       y = "Number of Employee") +
  theme_minimal()
termination_counts1 <- terminated_employees %>%
  group_by(GenderFull) %>%
  summarize(TotalTerminations = n())
termination_rates <- termination_counts1 %>%
  mutate(TotalEmployees = sum(TotalTerminations),
         TerminationRate = TotalTerminations / TotalEmployees * 100) %>%
  select(GenderFull, TerminationRate)
print(termination_rates)

## Analysis 2.8 - Terminations of employee by Department Name with Termination Reason
filtered_data <- empdata %>%
  filter(Status == "TERMINATED")  # Include only terminated employees
jobtitle_counts <- filtered_data %>%
  group_by(DepartmentName, TermReasonDesc) %>%
  count()
ggplot(jobtitle_counts, aes(x = DepartmentName, y = n, fill = TermReasonDesc)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), 
            color = "black", size = 2) +
  labs(title = "Department Name with Termination Reason",
       x = "Department Name",
       y = "Number of Employees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Analysis 2.9 - Terminations of employee by Job Title with Termination Reason
filtered_data <- empdata %>%
  filter(Status == "TERMINATED")  
# Count termination types
jobtitle_counts <- filtered_data %>%
  group_by(JobTitle, TermReasonDesc) %>%
  count()
ggplot(jobtitle_counts, aes(x = JobTitle, y = n, fill = TermReasonDesc)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), 
            color = "black", size = 2) +
  labs(title = "Job Title with Termination Reason",
       x = "Job Title",
       y = "Number of Terminations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## Analysis 2.10 – Terminations of employee by Store Name with Termination Reason
filtered_data <- empdata %>%
  filter(Status == "TERMINATED")  
storename_counts <- filtered_data %>%
  group_by(StoreName, TermReasonDesc) %>%
  count()
ggplot(storename_counts, aes(x = StoreName, y = n, fill = TermReasonDesc)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), 
            color = "black", size = 2) +
  labs(title = "Store Name with Termination Reasons",
       x = "Store Name",
       y = "Number of Terminations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous("Store Name", labels = storename_counts$StoreName, 
                     breaks = storename_counts$StoreName)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Question 3 - Which year has the highest termination, and why?
## Analysis 3.1 - Year with highest number of employee terminated
terminated_data <- empdata %>%
  filter(Status == "TERMINATED")
termination_counts <- terminated_data %>%
  group_by(StatusYear) %>%
  count()
highest_termination_count <- max(termination_counts$n)
highest_termination_year <- termination_counts %>%
  filter(n == highest_termination_count) %>%
  pull(StatusYear)
ggplot(termination_counts, aes(x = StatusYear, y = n,
                               fill = ifelse(n == highest_termination_count, "highlight", "normal"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(n == highest_termination_count, 
                               paste0("Highest: ", n), "")), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Number of Employee Terminations by Year",
       x = "Year",
       y = "Termination Count") +
  theme_minimal() +
  scale_fill_manual(values = c("highlight" = "red", "normal" = "steelblue")) +
  scale_x_continuous(breaks = seq(min(termination_counts$StatusYear), 
                                  max(termination_counts$StatusYear), by = 1))

## Analysis 3.2 - Ratio of Active Employees to Terminated Employees by Year
active_employees<- empdata %>%
  filter(Status == "ACTIVE")
terminated_employees<- empdata %>%
  filter(Status == "TERMINATED")
active_counts <- active_employees %>%
  group_by(StatusYear) %>%
  count()
terminated_counts <- terminated_employees %>%
  group_by(StatusYear) %>%
  count()
ratio_data <- active_counts %>%
  inner_join(terminated_counts, by = "StatusYear") %>%
  mutate(ratio = n.x / n.y)
ggplot(ratio_data, aes(x = as.factor(StatusYear), y = ratio)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Ratio of Active Employees to Terminated Employees by Year",
       x = "Year",
       y = "Ratio") +
  theme_minimal()

## Analysis 3.3 – Termination type and Termination reason in of employee 2014
data_2014 <- empdata %>%
  filter(StatusYear == 2014 & Status == "TERMINATED")
termreason_counts <- data_2014 %>%
  group_by(TermReasonDesc, TermTypeDesc) %>%
  summarise(count = n())
termreason_counts <- termreason_counts %>%
  arrange(desc(count))
ggplot(termreason_counts, aes(x = reorder(TermReasonDesc, -count), y = count, fill = TermTypeDesc)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +  # Add geom_text layer
  labs(title = "Termination Reason and Type Distribution in 2014",
       x = "Termination Reason",
       y = "Count",
       fill = "Termination Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "orange"),
                    guide = guide_legend(title = "Termination Type"))

## Analysis 3.4 - Ratio of termination reason of employee in 2014
terminated_2014 <- empdata %>%
  filter(StatusYear == 2014, Status == "TERMINATED")
termination_reason_counts <- terminated_2014 %>%
  group_by(TermReasonDesc) %>%
  count()
total_terminations <- sum(termination_reason_counts$n)
termination_reason_ratios <- termination_reason_counts %>%
  mutate(Ratio = n / total_terminations)
print(termination_reason_ratios)

## Analysis 3.5 - Age Range of employee layoff in 2014
layoffs_2014 <- empdata %>%
  filter(StatusYear == 2014, TermTypeDesc == "Involuntary", TermReasonDesc == "Layoff")
layoffs_2014 <- layoffs_2014 %>%
  mutate(AgeRange = cut(Age, breaks = c(0, 30, 40, 50, 60, Inf),
                        labels = c("<=30", "31-40", "41-50", "51-60", "60+")))
age_counts <- layoffs_2014 %>%
  group_by(AgeRange) %>%
  count()
ggplot(age_counts, aes(x = AgeRange, y = n, group = 1)) +
  geom_line(color = "pink") +
  geom_point(size = 3, color = "yellow") +
  geom_text(aes(label = n), vjust = -0.5, color = "black") +
  labs(title = "Age Range of Employee Layoffs in 2014",
       x = "Age Range",
       y = "Number of Layoffs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Analysis 3.6 - Department and Job Title of employee layoff in 2014
layoffs_2014 <- empdata %>%
  filter(StatusYear == 2014, TermTypeDesc == "Involuntary", TermReasonDesc == "Layoff")
layoff_counts <- layoffs_2014 %>%
  group_by(JobTitle, DepartmentName) %>%
  count()
ggplot(layoff_counts, aes(x = JobTitle, y = n, fill = DepartmentName)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = n), vjust = -0.5, color = "black") +
  labs(title = "Job Title and Department Layoffs in 2014",
       x = "Job Title",
       y = "Number of Layoffs",
       fill = "Department") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
print(layoff_counts)

## Analysis 3.7 – Store of employee layoff in 2014
layoffs_2014 <- empdata %>%
  filter(StatusYear == 2014, TermTypeDesc == "Involuntary", TermReasonDesc == "Layoff")
store_counts <- layoffs_2014 %>%
  group_by(StoreName) %>%
  count()
store_counts <- store_counts %>%
  filter(n > 0)
ggplot(store_counts, aes(x = reorder(StoreName, -n), y = n)) +
  geom_line(color = "skyblue") +
  geom_point(size = 3, color = "skyblue") +
  geom_text(aes(label = n), vjust = -0.5, color = "black") +
  labs(title = "Store of Employee Layoffs in 2014",
       x = "Store",
       y = "Number of Layoffs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Analysis 3.8 – City Name of employee layoff in 2014
layoffs_2014 <- empdata %>%
  filter(StatusYear == 2014,Status == "TERMINATED",TermTypeDesc == "Involuntary", TermReasonDesc == "Layoff")
city_counts <- layoffs_2014 %>%
  group_by(CityName) %>%
  count()
city_counts <- city_counts %>%
  filter(n > 0)
ggplot(city_counts, aes(x = reorder(CityName, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), vjust = -0.5, color = "black") +
  labs(title = "City of Employee Layoffs in 2014",
       x = "City",
       y = "Number of Layoffs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = city_counts$CityName)

## Analysis 3.9 – Gender distribution of employee layoff in 2014
layoffs_2014 <- empdata %>%
  filter(StatusYear == 2014, TermTypeDesc == "Involuntary", TermReasonDesc == "Layoff")
gender_counts <- layoffs_2014 %>%
  group_by(GenderFull) %>%
  count()
ggplot(gender_counts, aes(x = "", y = n, fill = GenderFull)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Gender Distribution of Employee Layoffs in 2014",
       fill = "Gender",
       x = NULL,
       y = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(n)), 
            position = position_stack(vjust = 0.5), 
            show.legend = FALSE)

## Analysis 3.10 - Length of Service of employees layoff in 2014
layoffs_2014 <- empdata %>%
  filter(StatusYear == 2014, TermTypeDesc == "Involuntary", TermReasonDesc == "Layoff")
layoffs_2014 <- layoffs_2014 %>%
  mutate(ServiceGroup = cut(LengthOfService, breaks = c(0, 5, 10, 15, 20, Inf),
                            labels = c("0-5", "6-10", "11-15", "16-20", "20+")))
service_counts <- layoffs_2014 %>%
  group_by(ServiceGroup) %>%
  summarise(Count = n())
ggplot(service_counts, aes(x = ServiceGroup, y = Count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Length of Service of Employees Laid Off in 2014",
       x = "Service Group",
       y = "Count") +
  theme_minimal()

## Question 4: What is the growth of employee based on business unit? 

## Analysis 4.1- Number employees needed for each business unit every year
store_employees <- empdata %>%
  filter(Status == "ACTIVE" & BusinessUnit == "STORES")
headoffice_employees <- empdata %>%
  filter(Status == "ACTIVE" & BusinessUnit == "HEADOFFICE")
plot1 <- ggplot(data = store_employees, aes(x = StatusYear)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 10) +
  labs(x = "Year", y = "Number of Employees", title = "Number of Employees in Store") +
  scale_x_continuous(breaks = seq(min(store_employees$StatusYear), max(store_employees$StatusYear), by = 2))
plot2 <- ggplot(data = headoffice_employees, aes(x = StatusYear)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 10) +
  labs(x = "Year", y = "Number of Employees", title = "Number of Employees in Head Office") +
  scale_x_continuous(breaks = seq(min(headoffice_employees$StatusYear), max(headoffice_employees$StatusYear), by = 2))
grid.arrange(plot1, plot2, nrow = 1)

## Analysis 4.2- Termination reason for the HEADOFFICE business unit every year
headoffice_employees <- empdata %>%
  filter(Status == "TERMINATED", BusinessUnit == "HEADOFFICE") %>%
  select(TermReasonDesc, TermTypeDesc)
termination_counts <- headoffice_employees %>%
  group_by(TermReasonDesc, TermTypeDesc) %>%
  summarise(count = n())
ggplot(termination_counts, aes(x = TermReasonDesc, y = count, fill = TermTypeDesc)) +
  geom_bar(stat = "identity") +
  xlab("Termination Reason") +
  ylab("Number of Employees") +
  ggtitle("Termination Reasons of Employees in HEADOFFICE by Termination Type") +
  theme(legend.position = "bottom")

## Analysis 4.3- Age Distribution of Retired Employees in HEADOFFICE Business Unit
headoffice_retired <- empdata %>%
  filter(Status == "TERMINATED", BusinessUnit == "HEADOFFICE", TermReasonDesc =="Retirement") %>%
  select(Age)
age_counts <- headoffice_retired %>%
  count(Age)
print(age_counts)

## Analysis 4.4- Average age of retirement in HEADOFFICE Business Unit
headoffice_retired <- empdata %>%
  filter(Status == "TERMINATED", BusinessUnit == "HEADOFFICE", TermReasonDesc =="Retirement") %>%
  select(Age)
average_age <- round(mean(headoffice_retired$Age),2)
print(average_age)

## Analysis 4.5- Average age of active employee under HEADOFFICE Business Unit for year 2015
headoffice_employees <- empdata %>%
  filter(BusinessUnit == "HEADOFFICE" & Status == "ACTIVE" & StatusYear == 2015)
age_data <- headoffice_employees %>%
  select(Age)
average_age <- round(mean(age_data$Age),2)
cat("Average Age:", average_age)

## Analysis 4.6 – Top three Department under STORE business unit that have highest growth number of employee from year 2006 to 2013
department_counts <- empdata %>%
  filter(Status == "ACTIVE" & BusinessUnit=="STORES") %>%
  group_by(DepartmentName, StatusYear) %>%
  count()
department_counts_wide <- department_counts %>%
  pivot_wider(names_from = StatusYear, values_from = n, values_fill = 0)
department_counts_diff <- department_counts_wide %>%
  mutate(increasing_rate = `2013` - `2006`) %>%
  arrange(desc(increasing_rate))
top_three_departments <- department_counts_diff %>%
  head(3)
print(top_three_departments$DepartmentName)

## Analysis 4.7 – Job title for the top three department
subset_data <- empdata %>%
  filter(DepartmentName %in% c("Customer Service", "Processed Foods", "Dairy"))
scatter_plot <- ggplot(subset_data, aes(x = DepartmentName, y = JobTitle)) +
  geom_point() +
  labs(x = "Department Name", y = "Job Title", title = "Relationship between Departments and Job Titles") +
  theme_minimal()
print(scatter_plot)

## Analysis 4.8 – Reason of termination of employee for business unit STORES from year 2014 to 2015
store_termination <- empdata %>%
  filter(StatusYear %in% c(2014, 2015), BusinessUnit == "STORES", Status == "TERMINATED") %>%
  group_by(TermReasonDesc,TermTypeDesc) %>%
  count()
ggplot(store_termination, aes(x = TermReasonDesc, y = n, fill = TermTypeDesc)) +
  geom_bar(stat = "identity") +
  labs(x = "Termination Reason", y = "Number of Employees", title = "Termination Reasons for STORES (2014-2015)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

## Analysis 4.9 – Department and job title of employee in STORES business unit that faced involuntary layoff issues from 2014 to 2015
stores_layoff_employees <- empdata %>%
  filter(BusinessUnit == "STORES" & Status == "TERMINATED" & TermReasonDesc == "Layoff",
         TermTypeDesc == "Involuntary",StatusYear %in% c(2014, 2015)) %>%
  select(DepartmentName, JobTitle)
counts <- stores_layoff_employees %>%
  count(DepartmentName, JobTitle, name = "Count")
ggplot(counts, aes(x = DepartmentName, y = Count, fill = JobTitle)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Department Name") +
  ylab("Count") +
  ggtitle("Departments and Job Titles for Involuntary Layoff Employees in STORES Business Unit over years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Analysis 4.10– Store name for employee that faced involuntary layoff issues in STORES business unit from 2014 to 2015
involuntary_layoff_employees <- empdata %>%
  filter(BusinessUnit == "STORES" & Status == "TERMINATED" & TermReasonDesc == "Layoff",
         TermTypeDesc == "Involuntary",StatusYear %in% c(2014, 2015)) %>%
  select(EmployeeID, StoreName)
store_counts <- involuntary_layoff_employees %>%
  count(StoreName)
store_counts <- store_counts[order(store_counts$n, decreasing = TRUE), ]
ggplot(store_counts, aes(x = reorder(StoreName, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("Store Name") +
  ylab("Number of Employees") +
  ggtitle("Number of Employees in Customer Service Department\nFacing Involuntary Layoff (STORES)") +
  theme_minimal() 

## Question 5: What is the growth of employee based on department? 

## Analysis 5.1 - Number of employee in each department from 2006 to 2015
department_counts <- empdata %>%
  filter(Status == "ACTIVE") %>%
  group_by(DepartmentName, StatusYear) %>%
  count()
department_counts_wide <- department_counts %>%
  pivot_wider(names_from = StatusYear, values_from = n, values_fill = 0)
print(department_counts_wide, n = Inf)

## Analysis 5.2 – Termination Reason of information technology department for year 2009
it_employees_2009 <- empdata %>%
  filter(Status == "TERMINATED" & DepartmentName == "Information Technology" & StatusYear == 2009)
termination_reason_counts <- it_employees_2009 %>%
  group_by(TermReasonDesc, JobTitle) %>%
  summarise(count = n())
termination_reason_counts <- termination_reason_counts %>%
  arrange(desc(count))
termination_reason_counts$TermReasonDesc[is.na(termination_reason_counts$TermReasonDesc)] <- "Unknown"
print(termination_reason_counts, n = Inf, na.print = "NA")

## Analysis 5.3 – Termination reason and job title of five departments from year 2014
departments <- c("Training", "Legal", "Labor Relations","Employee Records","HR Technology")
employees_2014 <- empdata %>%
  filter(Status == "TERMINATED" & DepartmentName %in% departments & StatusYear == 2014)
termination_reason_counts <- employees_2014 %>%
  count(DepartmentName, TermReasonDesc,JobTitle)
termination_reason_counts <- termination_reason_counts %>%
  arrange(desc(n))
termination_reason_counts$TermReasonDesc[is.na(termination_reason_counts$TermReasonDesc)] <- "Unknown"
print(termination_reason_counts, n = Inf, na.print = "NA")

## Analysis 5.4 – Termination reason and job title of six departments from year 2015
departments <- c("Accounting", "Accounts Payable", "Accounts Receiveable", "Audit", "Compensation", "Investment")
employees_2015 <- empdata %>%
  filter(Status == "TERMINATED" & DepartmentName %in% departments & StatusYear == 2015)
termination_reason_counts <- employees_2015 %>%
  group_by(DepartmentName, TermReasonDesc, JobTitle) %>%
  summarise(count = n())
termination_reason_counts <- termination_reason_counts %>%
  arrange(DepartmentName, desc(count))
termination_reason_counts$TermReasonDesc[is.na(termination_reason_counts$TermReasonDesc)] <- "Unknown"
print(termination_reason_counts, n = Inf, na.print = "NA")

## Analysis 5.5 – Average age and length of service of retirement in twelve departments 
departments <- c("Accounting", "Accounts Payable", "Accounts Receivable", "Audit", "Compensation", "Investment",
                 "Training", "Legal", "Labor Relations", "Employee Records", "HR Technology", "Information Technology")
employees_2015 <- empdata %>%
  filter(Status == "TERMINATED" & DepartmentName %in% departments) %>%
  select(DepartmentName, Age, LengthOfService)
average_age_LOS_retirement <- employees_2015 %>%
  group_by(DepartmentName) %>%
  summarise(AverageAge = mean(Age, na.rm = TRUE),
            AverageLOS = mean(LengthOfService, na.rm = TRUE))
print(average_age_LOS_retirement)
scatterplot <- ggplot(average_age_LOS_retirement, aes(x = AverageAge, y = AverageLOS, label = DepartmentName)) +
  geom_point(color = "blue", size = 3) +
  labs(x = "Average Age at Retirement", y = "Average Length of Service at Retirement", 
       title = "Average Age and Length of Service at Retirement by Department") +
  geom_text(hjust = 0, vjust = 0) +
  theme_minimal() +
  xlim(60, 66)
print(scatterplot)

## Analysis 5.6 – Top 3 department with the highest growth over years
department_counts <- empdata %>%
  filter(Status == "ACTIVE") %>%
  group_by(DepartmentName, StatusYear) %>%
  summarise(employee_count = n())
department_growth <- department_counts %>%
  group_by(DepartmentName) %>%
  arrange(StatusYear) %>%
  mutate(growth_rate = employee_count - lag(employee_count))
top_departments <- department_growth %>%
  filter(StatusYear > min(StatusYear)) %>%
  group_by(DepartmentName) %>%
  summarise(total_growth = sum(growth_rate)) %>%
  arrange(desc(total_growth)) %>%
  top_n(3)
print(top_departments)

## Analysis 5.7 – Age distribution of new hiring employee for top three departments
new_hire_data <- empdata %>%
  filter(DepartmentName %in% c("Customer Service", "Processed Foods", "Dairy"), Status == "ACTIVE") %>%
  mutate(HireDateO= year(HireDate))
ggplot(new_hire_data, aes(x = Age, fill = DepartmentName)) +
  geom_histogram(binwidth = 5, position = "identity") +
  facet_wrap(~DepartmentName, nrow = 1) +
  labs(x = "Age", y = "Count", title = "Age Distribution of New Hire Employees by Department") +
  scale_fill_manual(values = c("Customer Service" = "lightblue", "Processed Foods" = "lightgreen", "Dairy" = "pink")) +
  theme_minimal() +
  theme(legend.position = "bottom")

## Analysis 5.8 - Trend of job title under department with highest growth
filtered_data <- empdata %>%
  filter(DepartmentName %in% c("Customer Service", "Processed Foods", "Dairy"), Status=="ACTIVE")
job_counts <- filtered_data %>%
  group_by(DepartmentName, JobTitle, StatusYear) %>%
  summarise(employee_count = n())
ggplot(job_counts, aes(x = StatusYear, y = employee_count, color = JobTitle)) +
  geom_line() +
  facet_wrap(~ DepartmentName, ncol = 1) +
  xlab("Year") +
  ylab("Number of Employees") +
  ggtitle("Trend of Job Titles in Customer Service, Processed Food, and Dairy") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(job_counts$StatusYear), max(job_counts$StatusYear), by = 2))

## Analysis 5.9 – Store for departments with highest growth
employee_counts <- empdata %>%
  filter(Status == "ACTIVE", DepartmentName %in% c("Customer Service", "Processed Foods", "Dairy")) %>%
  group_by(DepartmentName, StoreName) %>%
  summarise(employee_count = n())
ggplot(employee_counts, aes(x = StoreName, y = employee_count, fill = DepartmentName)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ DepartmentName, ncol = 1) +
  labs(x = "Store Name", y = "Number of Employees", title = "Number of Employees in Each Store for Different Departments") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_x_continuous(breaks = 1:46, labels = as.character(1:46))

## Analysis 5.10 – Relationship between city and store name
city_store_data <- empdata %>%
  filter(Status == "ACTIVE") %>%
  select(CityName, StoreName)
scatterplot <- ggplot(city_store_data, aes(x = CityName, y = StoreName)) +
  geom_point() +
  labs(x = "City", y = "Store Name", title = "Relationship between City and Store Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = 1:46, labels = as.character(1:46))
print(scatterplot)





