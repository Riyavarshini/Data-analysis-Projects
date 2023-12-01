# Include required libraries
library(readxl)
library(tidyverse)
library(pivottabler)
# ---------- Import cleaned xlsx data ----
emp_details<-read_excel("Hr_emp_attrition_cleaned.xlsx")
view(emp_details)

# ---------- view summary of data ---
glimpse(emp_details)

summary(emp_details)
colnames(emp_details)

# Ordering the column values
emp_details$Age <- ordered(emp_details$Age,levels= c("Between 18 to 24","Between 25 to 31","Between 32 to 38","Between 39 to 44","Between 45 to 51","52 and more than 52"))
emp_details$Education <- ordered(emp_details$Education,levels = c("Below college","College","Bachelor","Master","Doctor"))
emp_details$Distance_From_Home <- ordered(emp_details$Distance_From_Home,levels = c("1 - 9 km","10 - 19 km","20 - 30 km"))
emp_details$Job_Involvement <- ordered(emp_details$Job_Involvement,levels = c("Low","Medium","High","Very High"))
emp_details$Environment_Satisfaction <- ordered(emp_details$Environment_Satisfaction,levels = c("Low","Medium","High","Very High"))
emp_details$Job_Satisfaction <- ordered(emp_details$Job_Satisfaction,levels = c("Low","Medium","High","Very High"))
emp_details$Relationship_Satisfaction <- ordered(emp_details$Relationship_Satisfaction,levels = c("Low","Medium","High","Very High"))
emp_details$Work_Life_Balance <- ordered(emp_details$Work_Life_Balance,levels = c("Bad","Good","Better","Best"))
emp_details$Total_Working_Years <- ordered(emp_details$Total_Working_Years,levels = c("0 - 10 years","11 - 20 years","21 - 30 years","More than 31 years"))
emp_details$Years_At_Company<- ordered(emp_details$Years_At_Company,levels = c("0 - 10 years","11 - 20 years","21 - 30 years","More than 31 years"))
emp_details$Years_In_Current_Role <- ordered(emp_details$Years_In_Current_Role,levels = c("0 - 5 years","6 - 10 years","11 - 15 years","16 - 20 years"))
emp_details$Years_Since_LastPromotion <- ordered(emp_details$Years_Since_LastPromotion,levels = c("0 - 5 years","6 - 10 years","11 - 15 years"))
emp_details$Years_with_Curr_Manager <- ordered(emp_details$Years_with_Curr_Manager,levels = c("0 - 5 years","6 - 10 years","11 - 15 years","16 - 20 years"))

# ---------- ANALYSIS
# 1. How many employees in the company are attrited?
attrit_details <- emp_details %>% 
  group_by(Attrition) %>% 
  summarise(No_of_employees = n())
print(attrit_details)

# 2. What is the overall attrition rate in company?
# ---------- Overall Attrition rate ( Attrition rate = no_of_employees/total employees)
attrit_details$percentage <- round((attrit_details$No_of_employees/(sum(attrit_details$No_of_employees)))*100,2)
print(attrit_details)

# 3. How is attrition affected by gender?

# ---------- Gender distribution in company
gender_attrition <- as.data.frame.matrix(emp_details %>% 
                            group_by(Gender) %>% 
                            select(Gender,Attrition) %>% 
                            table())
gender_attrition$total_employee <- gender_attrition$No+gender_attrition$Yes 
gender_attrition$attrition_rate <- round((gender_attrition$Yes/gender_attrition$total_employee)*100,2)
print(gender_attrition)
# findings: 
#     --> Males attrition rate is higher than female.

# 4. How does the hourly rate distribution vary by gender and attrition?

# ---------- HOURLY_RATE distribution by Gender - pivot table
qpvt(emp_details,"Gender","Attrition",c("minimum"="min(Hourly_Rate)","Q1"="quantile(Hourly_Rate,prob=.25, type=1)","Median"="round(median(Hourly_Rate),2)","Q3"="quantile(Hourly_Rate,prob=.75, type=1)","maximum"="max(Hourly_Rate)","Mean" = "round(mean(Hourly_Rate),2)"))
# Findings: 
#     --> females who left the company, on average, had a lower median hourly rate compared to their male colleagues.

# 5. How does the percent salary hike distribution vary by gender and attrition?

qpvt(emp_details,"Gender","Attrition",c("minimum"="min(Percent_Salary_Hike)","Q1"="quantile(Percent_Salary_Hike,prob=.25, type=1)","Median"="round(median(Percent_Salary_Hike),2)","Q3"="quantile(Percent_Salary_Hike,prob=.75, type=1)","maximum"="max(Percent_Salary_Hike)","Mean" = "round(mean(Percent_Salary_Hike),2)"))
#     --> The male employees who left the company had a lower average percent salary hike compared to their female counterparts.

# 6. How does the age of employees affect attrition?
Age_attrition <- as.data.frame.matrix(emp_details %>% 
                                           group_by(Age) %>% 
                                           select(Age,Attrition) %>% 
                                           table())
Age_attrition$total_employee <- Age_attrition$No+Age_attrition$Yes 
Age_attrition$attrition_rate <- round((Age_attrition$Yes/Age_attrition$total_employee)*100,2)
print(Age_attrition)
# Attrition rate of employees with age between 25 to 31 is found to be greater followed by age between 32 to 38 and age between 18 to 24.
# To sum up employee with ages between 18 - 38 are attritted mostly.

# 7. What are the attrition rates by female gender and age group?
Female_Age_attrition <-as.data.frame.matrix(emp_details %>% 
  filter(Gender == "Female") %>% 
  group_by(Age) %>% 
  select(Attrition) %>% 
  table())
Female_Age_attrition$total_employee <- Female_Age_attrition$No+Female_Age_attrition$Yes
Female_Age_attrition$attrition_rate <- round((Female_Age_attrition$Yes/Female_Age_attrition$total_employee)*100,2)
print(Female_Age_attrition)

# 8. attrition rates by Male and age group
Male_Age_attrition <-as.data.frame.matrix(emp_details %>% 
                                              filter(Gender == "Male") %>% 
                                              group_by(Age) %>% 
                                              select(Attrition) %>% 
                                              table())
Male_Age_attrition$total_employee <- Male_Age_attrition$No+Male_Age_attrition$Yes
Male_Age_attrition$attrition_rate <- round((Male_Age_attrition$Yes/Male_Age_attrition$total_employee)*100,2)
print(Male_Age_attrition)

# 9. How does business travel affect attrition?
btravel_attrition <- as.data.frame.matrix(emp_details %>% 
                                            group_by(Business_Travel) %>% 
                                            select(Attrition) %>% 
                                            table())
btravel_attrition$total_employee <- btravel_attrition$No+btravel_attrition$Yes 
btravel_attrition$attrition_rate <- round((btravel_attrition$Yes/btravel_attrition$total_employee)*100,2)
print(btravel_attrition)

# 10.How does distance from home to office affect attrition?
distance_attrition <- as.data.frame.matrix(emp_details %>% 
                                            group_by(Distance_From_Home) %>% 
                                            select(Attrition) %>% 
                                            table())
distance_attrition$total_employee <- distance_attrition$No+distance_attrition$Yes 
distance_attrition$attrition_rate <- round((distance_attrition$Yes/distance_attrition$total_employee)*100,2)
print(distance_attrition)

# 11. Which department has the highest attrition?
Department_attrition <- as.data.frame.matrix(emp_details %>% 
                                               group_by(Department) %>% 
                                               select(Attrition) %>% 
                                               table())
Department_attrition$total_employee <- Department_attrition$No + Department_attrition$Yes
Department_attrition$attrition_rate <- round((Department_attrition$Yes/Department_attrition$total_employee)*100,2)
print(Department_attrition)

# 12. Which job role has the highest attrition?
jobrole_attrition <- as.data.frame.matrix(emp_details %>% 
                                            group_by(Job_Role) %>% 
                                            select(Attrition) %>% 
                                            table())
jobrole_attrition$total_employee <- jobrole_attrition$No + jobrole_attrition$Yes
jobrole_attrition$attrition_rate <- round((jobrole_attrition$Yes/jobrole_attrition$total_employee)*100,2)
print(jobrole_attrition)

#13. How does employees' education have an effect on attrition?
education_attrition <- as.data.frame.matrix(emp_details %>% 
                           group_by(Education) %>% 
                           select(Attrition) %>% 
                           table())
education_attrition$total_employee <- education_attrition$No + education_attrition$Yes
education_attrition$attrition_rate <- round((education_attrition$Yes/education_attrition$total_employee)*100,2)
education_attrition

# 14. What are the attrition rates for each job role based on employees' education?
edu_job_attrition <- emp_details %>% 
  group_by(Education,Education_Field,Job_Role,Attrition) %>% 
  summarise(total_employee = n())


edu_job_attrition <- edu_job_attrition %>% 
  group_by(Education,Job_Role) %>% 
  mutate(total_by_job_role = sum(total_employee))

edu_job_attrition <- edu_job_attrition %>% 
  mutate(Attrition_rate = case_when(Attrition == "Yes" ~ round((total_employee/total_by_job_role)*100,2),
                                    Attrition == "No" ~ 0))
print(edu_job_attrition)

# 15. How does the education field affect attrition?
edu_field_attrition <- as.data.frame.matrix(emp_details %>% 
                                              group_by(Education_Field) %>% 
                                              select(Attrition) %>% 
                                              table())
edu_field_attrition <- edu_field_attrition %>% 
  mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
edu_field_attrition

# 16.What is the attrition rate of employees based on performance level?
performance <- as.data.frame.matrix(emp_details %>% 
                                      group_by(Performance_Rating) %>% 
                                      select(Attrition) %>% 
                                      table())
performance <- performance %>% 
  mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
performance

# 17. How does employee job involvement affect attrition?
job_involvement <- as.data.frame.matrix(emp_details %>% 
                                          group_by(Job_Involvement) %>% 
                                          select(Attrition) %>% 
                                          table())
job_involvement<- job_involvement %>% 
  mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))

job_involvement

# 18. Does overtime working lead to employee attrition?
overtime <- as.data.frame.matrix(emp_details %>% 
                                   group_by(Over_Time) %>% 
                                   select(Attrition) %>% 
                                   table())
overtime <- overtime %>% 
  mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
overtime

# 19. Does environmental satisfaction have any effect on attrition?
ev_satisfaction <- as.data.frame.matrix(emp_details %>% 
                                          group_by(Environment_Satisfaction) %>% 
                                          select(Attrition) %>%
                                          table())
ev_satisfaction <- ev_satisfaction %>%  mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
ev_satisfaction

# 20. Does job satisfaction have any effect on attrition?
job_satisfaction <- as.data.frame.matrix(emp_details %>% 
                                           group_by(Job_Satisfaction) %>% 
                                           select(Attrition) %>% 
                                           table())
job_satisfaction <- job_satisfaction %>% mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
job_satisfaction

# 21. How do job satisfaction and environmental satisfaction together affect attrition?
  group_by(Job_Satisfaction,Environment_Satisfaction,Attrition) %>% 
  summarise(total_employees = n())
x <- x %>% 
  group_by(Job_Satisfaction,Environment_Satisfaction) %>% 
  mutate(total_by_segment = sum(total_employees))
x <- x %>% 
  mutate(Attrition_rate = case_when(Attrition == "Yes" ~ round((total_employees/total_by_segment)*100,2),
                                    Attrition == "No" ~ 0))
x <- x %>% 
  group_by(Job_Satisfaction) %>% 
  arrange(desc(Attrition_rate))
x

# 22.How does work-life balance affect attrition?
work_life_balance<- as.data.frame.matrix(emp_details %>% 
                                           group_by(Work_Life_Balance) %>% 
                                           select(Attrition) %>% 
                                           table())
work_life_balance <- work_life_balance %>% mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
work_life_balance

# 23. How does the relationship with a manager affect attrition?
relationship_satisfaction <- as.data.frame.matrix(emp_details %>% 
                                                    group_by(Relationship_Satisfaction) %>% 
                                                    select(Attrition) %>% 
                                                    table())
relationship_satisfaction <- relationship_satisfaction %>% mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
relationship_satisfaction

# 24. What is the effect of years with the current manager on attrition?

years_with_curr_manager <- as.data.frame.matrix(emp_details %>% 
                                                    group_by(Years_with_Curr_Manager) %>% 
                                                    select(Attrition) %>% 
                                                    table())
years_with_curr_manager <- years_with_curr_manager %>% mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
years_with_curr_manager

# 25. Does monthly income have any effect on attrition?
qpvt(emp_details,"Monthly_Income","Attrition",c("minimum"="min(Monthly_Income)","Q1"="quantile(Monthly_Income,prob=.25, type=1)","Median"="round(median(Monthly_Income),2)","Q3"="quantile(Monthly_Income,prob=.75, type=1)","maximum"="max(Monthly_Income)","Mean" = "round(mean(Monthly_Income),2)"))

# 26. Does marital status have any effect on attrition?
gender_marital <- emp_details %>% 
  group_by(Gender,Marital_Status,Attrition) %>% 
  summarise(total_employees = n()) 

gender_marital <- gender_marital %>% 
  group_by(Gender,Marital_Status) %>% 
  mutate(total_by_marital_status = sum(total_employees))

gender_marital <- gender_marital %>% 
  mutate(Attrition_rate = case_when(
    Attrition == "Yes" ~ round((total_employees/total_by_marital_status)*100,2),
    Attrition == "No" ~ 0)) %>%
  arrange(Gender,desc(Attrition_rate))
gender_marital

# 27. Does the work experience of an employee have any effect on attrition?
work_experience <- as.data.frame.matrix(emp_details %>% 
                                           group_by(Total_Working_Years) %>% 
                                           select(Attrition) %>% 
                                           table())
work_experience <- work_experience %>% mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
work_experience

# 28. How does the number of years an employee is working in the current role affect attrition?
year_current_role <- as.data.frame.matrix(emp_details %>% 
                                               group_by(Years_In_Current_Role) %>% 
                                               select(Attrition) %>% 
                                               table())
year_current_role <- year_current_role %>% mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
year_current_role

# 29. 

# 30. Does the number of years working at the company have any effect on attrition?
years_At_company <- as.data.frame.matrix(emp_details %>% 
                                           group_by(Years_At_Company) %>% 
                                           select(Attrition) %>% 
                                           table())
years_At_company <- years_At_company %>% mutate(total_employee = No+Yes,attrition_rate = round((Yes/total_employee)*100,2))
years_At_company

# Export as a CSV file for Tableau visualization and dashboard creation
write_csv(emp_details,"attrition_viz.csv")
