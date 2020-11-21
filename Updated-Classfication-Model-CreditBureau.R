### Load Libraries ###
library(dplyr)
library(ggplot2)
library(Amelia)
library(caTools)
library(vtreat)

### Load Data Files ###
credit <- read.csv('Credit_Bureau.csv',header = T)
demo <- read.csv('demogs.csv',header = T)

### Check for Duplicated Records ###
sum(duplicated(credit$Application.ID))
length(unique(credit$Application.ID))

### Duplicated Records Extraction ###
demo %>% group_by(Application.ID) %>% filter(n()>1)
credit %>% group_by(Application.ID) %>% filter(n()>1)

demo <- demo %>% group_by(Application.ID) %>% filter(n()==1)
credit <- credit %>% group_by(Application.ID) %>% filter(n()==1)

### Merge the Data Files ###
merged_data <- merge(demo, credit, by = c("Application.ID","Performance.Tag"))
names(merged_data)[c(1:2, 5:6, 10:29)] <- c("Application_ID", "Performance_Tag", "Marital_Status", "No_Of_Dependents", "Type_Of_Residence", "Months_In_Current_Residence", "Months_In_Current_Company", "No_Of_90_DPD_6_months", "No_Of_60_DPD_6_months", "No_Of_30_DPD_6_months", "No_Of_90_DPD_12_months","No_Of_60_DPD_12_months","No_Of_30_DPD_12_months", "Avg_CC_Utilization_12_months", "Trades_6_months", "Trades_12_months", "PL_Trades_6_months", "PL_Trades_12_months", "Inquiries_6_months", "Inquiries_12_months", "Open_Home_Loan", "Outstanding_Balance", "Total_No_of_trades", "Open_Auto_Loan")
merged_data <- select(merged_data,-Application_ID)
str(merged_data)

### Check for Missing Values in the Response Variable ###
merged_data$Performance_Tag %>% is.na() %>% sum()

### Check and Drop Missing Data ###
missmap(merged_data)
merged_data <- na.omit(merged_data)
# We can see from the missmap that missing data are not too much, so I choose to drop them. #

### Plot for Performance Tag ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=as.factor(Performance_Tag), y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar() +
  labs(x="Performance Tag", y="Frequency in 1000s", fill="Performance Tag", title="Frequency of Performance Tag") +
  theme_minimal()
# We can see the frequency of non-default count is way higher than that of default count #

### Percentage of Default ###
temp <- table(merged_data$Performance_Tag)
default_count <- temp[2]
nondefault_count <- temp[1]
default_percent <- default_count/(default_count+nondefault_count)
default_percent
# 4.21%

### Convert Character Variables into Factor Variables
merged_data$Performance_Tag <- sapply(merged_data$Performance_Tag,factor)
merged_data$Gender <- sapply(merged_data$Gender,factor)
merged_data$No_Of_Dependents <- sapply(merged_data$No_Of_Dependents,factor)
merged_data$Marital_Status<- sapply(merged_data$Marital_Status,factor)
merged_data$Education <- sapply(merged_data$Education,factor)
merged_data$Profession <- sapply(merged_data$Profession,factor)
merged_data$Type_Of_Residence <- sapply(merged_data$Type_Of_Residence,factor)
str(merged_data)

#############################
# Exploratory Data Analysis #
#############################

#=========#
#   Age   #
#=========#
merged_data$Age %>% quantile(seq(0,1,0.01))
merged_data$Age %>% boxplot()
merged_data[(which(merged_data$Age<18)),]$Age <- 18
merged_data$Age %>% quantile(seq(0,1,0.01))

### Create Bins on Age ###
age_bin <- function(age){
  if(age > 0 && age < 21)
    return ("18-20")
  else if(age > 20 && age < 26)
    return ("21-25")
  else if(age > 25 && age < 31)
    return ("26-30")
  else if(age > 30 && age < 36)
    return ("31-35")
  else if(age > 35 && age < 41)
    return ("36-40")
  else if(age > 40 && age < 46)
    return ("41-45")
  else if(age > 45 && age < 51)
    return ("46-50")
  else if(age > 50 && age < 56)
    return ("51-55")
  else if(age > 55 && age < 61)
    return ("56-60")
  else if(age > 60 && age < 66)
    return ("60-65")
}

### Create Age Bin field ###
merged_data$Age_Bin <- merged_data$Age %>% sapply(age_bin) %>% as.factor()
attributes(merged_data$Age_Bin)

### Plot for Frequency of Age Bins ###
ggplot(merged_data, aes(x=Age_Bin, y=..count../1000, fill=Age_Bin)) +
  geom_bar() +
  labs(x="Age Bin", y="Frequency in 1000s", fill="Age Bin", title="Frequency of different Age Bins") +
  theme_minimal()
# We can see the age groups of 36 to 55 have higher frequency than other groups #

### Age Bucket wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Age_Bin, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Age Buckets", y="Frequency in 1000s", fill="Performance Tag", title="Age Bucket wise Performance Tag Frequency")
# The age groups of 36 to 55 also have higher default frequency than other groups


#=============#
#   Gender    #
#=============#
merged_data$Gender %>%
  summary()
# 2 NA's

### Converting NA for Gender variable to "M" ###
levels(merged_data$Gender)[3] <- "M"

### Plot for frequency of each Gender ###
ggplot(merged_data, aes(x=Gender, y=..count../1000, fill=Gender)) +
  geom_bar() +
  labs(x="Gender", y="Frequency in 1000s", fill="Gender", title="Frequency of different Gender") +
  theme_minimal()
# The frequency of Male is more than double of that of female #

### Gender wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Gender, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Gender", y="Frequency in 1000s", fill="Performance Tag", title="Gender wise Performance Tag Frequency")
# The frequency of Male default is also higher than that of female default #

#=====================#
#   Marital Status   #
#=====================#
merged_data$Marital_Status %>%
  summary()
# 6 NA's

### Converting NA for Marital status at time of application variable to "Married" ###
levels(merged_data$Marital_Status)[3] <- "Married"

### Plot for Marital status at time of application frequency ###
ggplot(merged_data, aes(x=Marital_Status, y=..count../1000, fill=Marital_Status)) +
  geom_bar()+
  labs(x="Marital Status at time of application", y="Frequency in 1000s", fill="Marital Status", title="Frequency of different Marital Status") +
  theme_minimal()
# The frequency of Married applicants is much higher than that of Single applicants #

### Marital Status wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Marital_Status, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Marital Status", y="Frequency in 1000s", fill="Performance Tag", title="Marital Status wise Performance Tag Frequency")
# The default frequency of Married applicants is also much higher than that of Single applicants #



#=====================#
#   No of Dependents  #
#=====================#
### Checking for NA values ###
merged_data$No_Of_Dependents %>%
  is.na() %>%
  sum()

# 6 NA's

merged_data$No_Of_Dependents[which(is.na(merged_data$No_Of_Dependents))] <- 3

merged_data$No_Of_Dependents %>%
  as.factor() %>%
  summary()

### Checking for outliers ###
merged_data$No_Of_Dependents %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

### Converting the variable into factor type ###
merged_data$No_Of_Dependents <- merged_data$No_Of_Dependents %>% as.factor()

### Plot for No of Dependents Frequency ###
ggplot(merged_data, aes(x=as.factor(No_Of_Dependents), y=..count../1000, fill=as.factor(No_Of_Dependents))) +
  geom_bar() +
  labs(x="No of Dependents", y="Frequency in 1000s", fill="No of Dependents", title="Frequency of No of Dependents") +
  theme_minimal()
# Applicants who have one to three dependents are more than those who have four or five dependents #

### No of Dependents wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=No_Of_Dependents, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="No of Dependents", y="Frequency in 1000s", fill="Performance Tag", title="No of Dependents wise Performance Tag Frequency")
# Applicants who have one to three dependents have higher default number than those who have four or five dependents #

#=============#
#   Income    #
#=============#
### checking for NA values ###
merged_data$Income %>%
  is.na() %>%
  sum()
# 0

### Checking for outliers ###
merged_data$Income %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data$Income %>%
  as.factor() %>%
  levels()

### Converting Income less than 1 to 1.0 ###
merged_data[(which(merged_data$Income < 1)), ] $Income <- 1.0

### Creating Income Bracket ###
# Income Bracket Function

income_bin <- function(income = 1){
  if(income >= 1 && income <=10)
    return ("1-10")
  else if(income >= 11 && income <=20)
    return ("11-20")
  else if(income >= 21 && income <=30)
    return ("21-30")
  else if(income >= 31 && income <=40)
    return ("31-40")
  else if(income >= 41 && income <=50)
    return ("41-50")
  else
    return ("51-60")
}


merged_data$Income_Bin <-  merged_data$Income %>%
  sapply(income_bin) %>%
  as.factor()

### Income Bucket wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Income_Bin, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Income Buckets", y="Frequency in 1000s", fill="Performance Tag", title="Income Bucket wise Performance Tag Frequency")
# We can see a negative relationship between the income bucket and the default number #

#===============#
#   Education   #
#===============#
### checking for NA values ###
merged_data$Education %>%
  is.na() %>%
  sum()

# 0

### Checking for blank rows ###
merged_data$Education %>%
  summary()

levels(merged_data$Education)[6] <- "Professional"

### Plot for Education Frequency ###
ggplot(merged_data, aes(x=Education, y=..count../1000, fill=Education)) +
  geom_bar() +
  labs(x="Education", y="Frequency in 1000s", fill="Education", title="Frequency of Education") +
  theme_minimal()
# The applicants with professional education background have the highest frequency #

### Education wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Education, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Education", y="Frequency in 1000s", fill="Performance Tag", title="Education wise Performance Tag Frequency")
# The applicants with professional education background also have the highest default frequency #


#=================#
#   Profession    #
#=================#

### checking for NA values ###
merged_data$Profession %>%
  is.na() %>%
  sum()

# 0

### Checking for blank rows ###
merged_data$Profession %>%
  summary()

levels(merged_data$Profession)[4] <- "SAL"

### Plot for Profession Frequency ###
ggplot(merged_data, aes(x=Profession, y=..count../1000, fill=Profession)) +
  geom_bar() +
  labs(x="Profession", y="Frequency in 1000s", fill="Profession", title="Frequency of Profession") +
  theme_minimal()
# Applicants with SAL profession have the highest number among three professions #

### Profession wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Profession, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Profession", y="Frequency in 1000s", fill="Performance Tag", title="Profession wise Performance Tag Frequency")
# Applicants with SAL profession also have the highest default number among three professions #


#=======================#
#   Type of residence   #
#=======================#
### checking for NA values ###
merged_data$Type_Of_Residence %>%
  is.na() %>%
  sum()

# 0

### Checking for blank rows ###
merged_data$Type_Of_Residence %>%
  summary()

levels(merged_data$Type_Of_Residence)[6] <- "Rented"

### Plot for frequency of type of residence ###
ggplot(merged_data, aes(x=Type_Of_Residence, y=..count../1000, fill=Type_Of_Residence)) +
  geom_bar() +
  labs(x="Type of residence", y="Frequency in 1000s", fill="Type of residence", title="Frequency of Type of residence") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# Applicants whose residence type is rented have the highest frequency among five categories#

### Type of Residence wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Type_Of_Residence, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  labs(x="Type of Residence", y="Frequency in 1000s", fill="Performance Tag", title="Type of Residence wise Performance Tag Frequency") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# Applicants whose residence type is rented also have the highest default frequency among five categories#


#===========================================#
#   Number of months in current residence   #
#===========================================#

### Checking for NA values ###
merged_data$Months_In_Current_Residence %>%
  is.na() %>%
  sum()

# 0

### Checking for outliers ###
merged_data$Months_In_Current_Residence %>%
  quantile(seq(0,1,0.01), na.rm = T)

merged_data$Months_In_Current_Residence %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")


### Resident Years Bin Function ###
res_yrs_bin <- function(nom=0){
  noy = nom/12
  if(noy > 0 && noy < 1)
    return("< 1 yr")
  else if(noy >= 1 && noy < 2)
    return("1 yr")
  else if(noy >= 2 && noy < 3)
    return("2 yrs")
  else if(noy >= 3 && noy < 4)
    return("3 yrs")
  else if(noy >= 4 && noy < 5)
    return("4 yrs")
  else if(noy >= 5 && noy < 6)
    return("5 yrs")
  else if(noy >= 6 && noy < 7)
    return("6 yrs")
  else if(noy >= 7 && noy < 8)
    return("7 yrs")
  else if(noy >= 8 && noy < 9)
    return("8 yrs")
  else if(noy >= 9 && noy < 10)
    return("9 yrs")
  else
    return("> 10 yrs")
}

### Creating No of years in current residence variable ###
merged_data$Yrs_Curr_Res <- merged_data$Months_In_Current_Residence %>%
  sapply(res_yrs_bin) %>%
  as.factor()

### Plot of frequency of No of years in current residence variable ###
ggplot(merged_data, aes(x=Yrs_Curr_Res, y=..count../1000, fill=Yrs_Curr_Res)) +
  geom_bar() +
  labs(x="No of Years in residence", y="Frequency in 1000s", fill="No of Years in residence", title="Frequency of Years in residence") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# Applicants with less than one year in residence have the highest frequency among all categories #

### Years In Current Residence wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Yrs_Curr_Res, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  labs(x="Years In Current Residence", y="Frequency in 1000s", fill="Performance Tag", title="Years In Current Residence wise Performance") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# Applicants with less than one year in residence also have the highest default frequency among all categories #


#=========================================#
#   Number of months in current company   #
#=========================================#

### Checking for NA values ###
merged_data$Months_In_Current_Company %>%
  is.na() %>%
  sum()

# 0

### Checking for outliers ###
merged_data$Months_In_Current_Company %>%
  quantile(seq(0,1,0.01), na.rm = T)


merged_data$Months_In_Current_Company %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

### Capping No of months in current company to 74 ###
merged_data[(which(merged_data$Months_In_Current_Company > 74)),] $Months_In_Current_Company <- 74

### Current Company Years Bin Function ###
comp_yrs_bin <- function(nom=0){
  noy = nom/12
  if(noy > 0 && noy < 1)
    return("< 1 yr")
  else if(noy >= 1 && noy < 2)
    return("1 yr")
  else if(noy >= 2 && noy < 3)
    return("2 yrs")
  else if(noy >= 3 && noy < 4)
    return("3 yrs")
  else if(noy >= 4 && noy < 5)
    return("4 yrs")
  else if(noy >= 5 && noy < 6)
    return("5 yrs")
  else
    return("> 6 yrs")
}

### Crating variable No of years in curr comp ###
merged_data$Yrs_Curr_Comp <- merged_data$Months_In_Current_Company %>%
  sapply(comp_yrs_bin) %>%
  as.factor()

### Plot for No of years in current company ###
ggplot(merged_data, aes(x=Yrs_Curr_Comp, y=..count../1000, fill=Yrs_Curr_Comp)) +
  geom_bar() +
  labs(x="No of Years in Current Company", y="Frequency in 1000s", fill="No of Years in Current Company", title="Frequency of Years in Current Company") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# There is no obvious trend between years in current company and frequency #

### Years In Current Company wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Yrs_Curr_Comp, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Years In Current Company", y="Frequency in 1000s", fill="Performance Tag", title="Years In Current Company wise Performance Tag Frequency")
# There is no obvious trend between years in current company and default frequency #


#===================================================#
#   No of times 90 DPD or worse in last 6 months    #
#===================================================#

### Checking for NA values ###
merged_data$No_Of_90_DPD_6_months %>%
  is.na() %>%
  sum()

# 0

merged_data$No_Of_90_DPD_6_months %>%
  as.factor() %>%
  summary()


#===================================================#
#   No of times 60 DPD or worse in last 6 months    #
#===================================================#

### Checking for NA values ###
merged_data$No_Of_60_DPD_6_months %>%
  is.na() %>%
  sum()

# 0

merged_data$No_Of_60_DPD_6_months %>%
  as.factor() %>%
  summary()



#===================================================#
#   No of times 30 DPD or worse in last 6 months    #
#===================================================#

### Checking for NA values ###
merged_data$No_Of_30_DPD_6_months %>%
  is.na() %>%
  sum()

# 0

merged_data$No_Of_30_DPD_6_months %>%
  as.factor() %>%
  summary()



#===================================================#
#   No of times 90 DPD or worse in last 12 months   #
#===================================================#

### Checking for NA values ###
merged_data$No_Of_90_DPD_12_months %>%
  is.na() %>%
  sum()

# 0

merged_data$No_Of_90_DPD_12_months %>%
  as.factor() %>%
  summary()



#===================================================#
#   No of times 60 DPD or worse in last 12 months   #
#===================================================#

### Checking for NA values ###
merged_data$No_Of_60_DPD_12_months %>%
  is.na() %>%
  sum()

# 0

merged_data$No_Of_60_DPD_12_months %>%
  as.factor() %>%
  summary()



#===================================================#
#   No of times 30 DPD or worse in last 12 months   #
#===================================================#

### Checking for NA values ###
merged_data$No_Of_30_DPD_12_months %>%
  is.na() %>%
  sum()
# 0

merged_data$No_Of_30_DPD_12_months %>%
  as.factor() %>%
  summary()

#===================================#
#   Correlation of DPD Variables    #
#===================================#

DPD_data_6 <- merged_data[, c(13:15)]
DPD_data_12 <- merged_data[, c(16:18)]

cor_DPD_6 <- round(cor(DPD_data_6), 2)
cor_DPD_6
melted_cor_DPD_6 <- melt(cor_DPD_6)

cor_DPD_12 <- round(cor(DPD_data_12), 2)
melted_cor_DPD_12 <- melt(cor_DPD_12)

# DPD Correlation heat map for 6 months
ggplot(melted_cor_DPD_6, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  labs(x="", y="", title="DPD 6 months Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# No of 30 DPD in 6 months has a relatively high correlation with No of 60 DPD in 6 months #

# DPD Correlation heat map for 12 months
ggplot(melted_cor_DPD_12, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  labs(x="", y="", title="DPD 12 months Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# No of 30 DPD in 12 months has a relatively high correlation with No of 30 DPD in 6 months #
# AVG CC utilization in 12 months has relatively low correlations with other two variables #


#=======================================================#
#   Average Credit Card utilisation in last 12 months   #
#=======================================================#

### Checking for NA values ###
merged_data$Avg_CC_Utilization_12_months %>%
  is.na() %>%
  sum()
# 0

merged_data$Avg_CC_Utilization_12_months %>%
  summary()

### Checking for outliers ###
merged_data$Avg_CC_Utilization_12_months %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Avg_CC_Utilization_12_months %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Avg_CC_Utilization_12_months > 103)),] $Avg_CC_Utilization_12_months <- 103




#==========================================#
#   No of trades opened in last 6 months   #
#==========================================#

### Checking for NA values ###
merged_data$Trades_6_months %>%
  is.na() %>%
  sum()

# 0

merged_data$Trades_6_months %>%
  summary()

### Checking for outliers ###
merged_data$Trades_6_months %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Trades_6_months %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Trades_6_months > 6)),] $Trades_6_months <- 6



#===========================================#
#   No of trades opened in last 12 months   #
#===========================================#

### Checking for NA values ###
merged_data$Trades_12_months %>%
  is.na() %>%
  sum()

# 0

### Checking for outliers ###
merged_data$Trades_12_months %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Trades_12_months %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Trades_12_months > 19)),] $Trades_12_months <- 19

#===================================#
#   Correlation of trades opened    #
#===================================#

trades_opened <- merged_data[, c(20, 21)]

cor_trades_opened <- round(cor(trades_opened), 2)
melted_cor_trades_opened <- melt(cor_trades_opened)

### DPD Correlation heat map for 6 months ###
ggplot(melted_cor_trades_opened, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  labs(x="", y="", title="Trades Opened Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# PL trades opened in 6 months has a high correlation with trades opened in 12 months #

#==============================================#
#   No of PL trades opened in last 6 months    #
#==============================================#

### Checking for NA values ###
merged_data$PL_Trades_6_months  %>%
  is.na() %>%
  sum()

# 0

### Checking for outliers ###
merged_data$PL_Trades_6_months  %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$PL_Trades_6_months  %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$PL_Trades_6_months  > 5)),] $PL_Trades_6_months  <- 5



#===============================================#
#   No of PL trades opened in last 12 months    #
#===============================================#

### Checking for NA values ###
merged_data$PL_Trades_12_months  %>%
  is.na() %>%
  sum()

# 0

### Checking for outliers ###
merged_data$PL_Trades_12_months  %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$PL_Trades_12_months  %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$PL_Trades_12_months  > 10)),] $PL_Trades_12_months  <- 10

#===================================#
#   Correlation of PL trades opened    #
#===================================#

pl_trades_opened <- merged_data[, c(22, 23)]

cor_pl_trades_opened <- round(cor(pl_trades_opened), 2)
melted_cor_pl_trades_opened <- melt(cor_pl_trades_opened)

### DPD Correlation heat map for 6 months ###
ggplot(melted_cor_pl_trades_opened, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  labs(x="", y="", title="PL Trades Opened Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))
# PL trades opened in 12 months has a high correlation with inquiries made in 12 months #


#===============================================================#
#   No if inquiries in last 6 months excluding home auto loan   #
#===============================================================#

### Checking for NA values ###
merged_data$Inquiries_6_months %>%
  is.na() %>%
  sum()

# 0

### Checking for outliers ###
merged_data$Inquiries_6_months %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Inquiries_6_months %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Inquiries_6_months > 7)),] $Inquiries_6_months <- 7



#=================================================================#
#   No if inquiries in last 12 months excluding home auto loan    #
#=================================================================#

### Checking for NA values ###
merged_data$Inquiries_12_months %>%
  is.na() %>%
  sum()

# 0

### Checking for outliers ###
merged_data$Inquiries_12_months %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Inquiries_12_months %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Inquiries_12_months > 12)),] $Inquiries_12_months <- 12




#=================================#
#   Presence of open home loan    #
#=================================#

### Checking for NA values ###
merged_data$Open_Home_Loan %>%
  is.na() %>%
  sum()

# 272

merged_data$Open_Home_Loan %>%
  as.factor() %>%
  summary()

### Converting to factor type ###
merged_data$Open_Home_Loan <- merged_data$Open_Home_Loan %>%
  as.factor()

# Plot for  Presence of open home loan
ggplot(merged_data, aes(x=Open_Home_Loan, y=..count../1000, fill=Open_Home_Loan)) +
  geom_bar() +
  labs(x="Presence of open home loan ", y="Frequency in 1000s",fill="Presence of open home loan ", title="Frequency of Presence of open home loan ") +
  theme_minimal()
# The presence of open home loan is more than the non-presence of that loan #

# Open Home Loan wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Open_Home_Loan, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Presence of Open Home Loan", y="Frequency in 1000s", fill="Performance Tag", title="Open Home Loan wise Performance Tag Frequency")
# Applicants with the presence of open home loan has more more default number than the applicants with non-presence of that loan #



#=================================#
#   Presence of open auto loan    #
#=================================#

### Checking for NA values ###
merged_data$Open_Auto_Loan %>%
  is.na() %>%
  sum()

# 0

merged_data$Open_Auto_Loan %>%
  as.factor() %>%
  summary()

### Converting to factor type ###
merged_data$Open_Auto_Loan <- merged_data$Open_Auto_Loan %>%
  as.factor()

### Plot for  Presence of open auto loan ###
ggplot(merged_data, aes(x=Open_Auto_Loan, y=..count../1000, fill=Open_Auto_Loan)) +
  geom_bar() +
  labs(x="Presence of open auto loan ", y="Frequency in 1000s",fill="Presence of open auto loan ", title="Frequency of Presence of open auto loan ") +
  theme_minimal()
# Applicants with the opened auto loan has more frequency than the applicants with non-opened auto loan #

### Open Auto Loan wise Performance Tag Frequency ###
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Open_Auto_Loan, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Presence of Open Auto Loan", y="Frequency in 1000s", fill="Performance Tag", title="Open Auto Loan wise Performance Tag Frequency")
# Applicants with the opened auto loan has more default frequency than the applicants with non-opened auto loan #


#=========================#
#   Outstanding Balance   #
#=========================#

### Checking for NA values ###
merged_data$Outstanding_Balance %>%
  is.na() %>%
  sum()
# 0

### Checking for outliers ###
merged_data$Outstanding_Balance %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Outstanding_Balance %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")


#=========================#
#   Total no of trades    #
#=========================#

### Checking for NA values ###
merged_data$Total_No_of_trades %>%
  is.na() %>%
  sum()
# 0

# Checking for outliers
merged_data$Total_No_of_trades %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Total_No_of_trades %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Total_No_of_trades > 20)),] $Total_No_of_trades <- 20



#############################
# LOGISTIC REGRESSION MODEL #
#############################
str(merged_data)

### Train Test Split ###
set.seed(101)

sample <- sample.split(merged_data$Performance_Tag, SplitRatio = 0.7)

### Train ###
train <- subset(merged_data, sample == T)

### Test ###
test <- subset(merged_data, sample == F)

### Model ###
model <- glm(Performance_Tag ~ . , family = binomial(link = 'logit'), data = train)
summary(model)
# We can see from the summary of the regression model that there exist many insignificant variables since the P-value of them are relatively large.

### Step Function ###
new.step.model <- step(model)
summary(new.step.model)
# I used the step function to just keep those significant variables in the new model #

### Use test data ###
test$predicted_Performance_Tag <- predict(new.step.model,newdata = test, type = 'response')

### Confusion Matrix ###
table(test$Performance_Tag, test$predicted_Performance_Tag > 0.5)

### Accuracy ###
acc <- 19782/(19782 + 870)
# 95.8%

### Using 10-fold cross-validation ###
splitPlan <- kWayCrossValidation(nrow(merged_data),10,NULL,NULL)

### Examine the split plan ###
str(splitPlan)

### Run the 10-fold cross validation plan from splitPlan ###
k <- 10 # Number of folds
merged_data$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model_cv <- glm(Performance_Tag ~ . , family = binomial(link = 'logit'), data = merged_data[split$train, ])
  merged_data$pred.cv[split$app] <- predict(model_cv, newdata = merged_data[split$app, ])
}

### Confusion Matrix ###
table(merged_data$Performance_Tag, merged_data$pred.cv > 0.5)

### Accuracy ###
acc_cv <- 65939/(65939 + 2899)
# 95.8%


########################################################################################
# After running the step function, I got a new logistic regression model.              #
# From the summary of this new model, I found that P-values of these variables         #
# are less than 5% : ProfessionSAL , No_Of_60_DPD_6_months , No_Of_30_DPD_6_months ,   #
# No_Of_90_DPD_12_months, Avg_CC_Utilization_12_months, PL_Trades_12_months ,          #
# Inquiries_12_months , and Total_No_of_trades. I consider these variables as          #
# significant variables to predict if a new applicant will have a high possibility     #
# of default.                                                                          #
#                                                                                      #
# After applying this logistic regression model into the test data, I compared         #
# the predicted value of performance tag with the performance tag value in the         #
# test data. Also, I used 10-fold cross-validation to check the model.                 #    
# Then, I called the confusion matrix and calculated the accuracy as                   #
# 95.8%, which means this logistic regression model can be considered as a relatively  #
# good fit to predict the default.                                                     #
#                                                                                      #
########################################################################################
