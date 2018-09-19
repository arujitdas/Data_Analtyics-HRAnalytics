############################# HR Case Study Solution ###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Company XYZ wants to know what changes they should make to their workplace, in order to 
# get most of their employees to stay. Also, they want to know which of these variables 
# are most important and needs to be addressed right away.

## AIM:
# To model the probability of attrition using a logistic regression

## Outcome
# The results thus obtained will be used by the management to understand what changes 
# they should make to their workplace, in order to get most of their employees to stay.

############################ Load the required libraries  ###################################
library(plyr)
library(dplyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(ROCR)

###################load all the CSV dataset files in environment ############################
employee_survey_data<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_survey_data<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
general_data<-read.csv("general_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

############### high level analysis on the structure of dataset ######################
str(employee_survey_data)    
str(manager_survey_data)    
str(general_data)    
str(in_time)    
str(out_time)    
#all have 4410 variables

####################### check for uniqueness ###########################################
length(unique(tolower(employee_survey_data$EmployeeID)))  
length(unique(tolower(manager_survey_data$EmployeeID))) 
length(unique(tolower(general_data$EmployeeID)))
length(unique(tolower(in_time$X))) 
length(unique(tolower(out_time$X))) 
# Data is unique in each dataset

################# Derived metrics and observations #################################

# Observation : If we look into the in_time and out_time dataset, it is difficult to
# analyze. However, we can take these two dataset and find out some derived metrics
# such as average work time. this may give some insights on the attrition rate

#calculate the average working hours per employee using out_time and in_time

# Assumption : First column of this dataset doesn't have column name. However, it looks
# like an Employee ID. Let us finish all conversion and then assign name to this column
#converting in_time columns to date time format except employee ID i.e. first column
in_time_exceptEmpID <- sapply(in_time[,-1], function(x) as.POSIXlt(x, origin="1970-01-01", format = "%Y-%m-%d %H:%M:%S"))
in_time_exceptEmpID <- as.data.frame(in_time_exceptEmpID)
in_time<-cbind(in_time[,1],in_time_exceptEmpID)
colnames(in_time)[1] <- "EmployeeID"


# Assumption : First column of this dataset doesn't have column name. However, it looks
# like an Employee ID. Let us finish all conversion and then assign name to this column
#converting in_time columns to date time format except employee ID i.e. first column
out_time_exceptEmpID <- sapply(out_time[,-1], function(x) as.POSIXlt(x, origin="1970-01-01",format = "%Y-%m-%d %H:%M:%S"))
out_time_exceptEmpID<-as.data.frame(out_time_exceptEmpID)
out_time<-cbind(out_time[,1],out_time_exceptEmpID)
colnames(out_time)[1] <- "EmployeeID"

#calculate the difference between out time and in time. Exclude the emp id column
diff<-out_time[,-1]-in_time[,-1]

############################### Custom Functions ####################################################

# Write two functions separately which will look for each column of the dataset and find out if  
# all the values in column is null or empty. If yes, we can remove the complete column as it 
# will not have any significance in our analysis

# Function to retrieve all the columns with all N/A values
AllNA.test <-  function (x) {
  w <- sapply(x, function(x)all(is.na(x)))
  if (any(w)) {
    # Columns with all NA values are listed here
    #paste("All NA in columns", paste(which(w), collapse=", "))
    return(which(w))
  }
}

# Function to retrieve all the columns with all Empty values
AllEmpty.test <- function(x) {
  w <- sapply(x, function(x)all(x==""))
  if (any(w)) {
    # Columns with all Empty values are listed here
    paste("All Empty in columns", paste(which(w), collapse=", "))
    return(which(w))
  }
}

# Function to retrieve all the columns with only one unique value. If there is any such column then
# this column is not significant for any analysis
SingleUniqueValue.test <- function(x) {
  w <- sapply(x, function(x)length(unique(x)) == 1)
  if (any(w)) {
    return(which(w))
  }
}

##############################################################################################

# Call the custom function and remove all columns having all NA values
x <- AllNA.test(diff)
diff <- diff[, - c(x)]

# convert the diff dataframe into numeric for easy calculation
diff<-sapply(diff,function(x) as.numeric(x))
diff<-as.data.frame(diff)

# lets find the average working hour
diff$AvgWorkTime<-apply(diff,1,mean,na.rm=TRUE)

#Let's calculate number of days each employees has worked more than 8 hours
funcOvertime<-function(x){return(sum(x>8,na.rm=TRUE))}
# adding -250 here as this column refers to the average work time, which we don't want to consider
diff$OverTimeDays<-apply(diff[-250],1,funcOvertime)

# create a dataframe with employee id,  AvgWorkTime and OverTimeDays column
AvgWorkTime<-cbind(in_time[,1],diff$AvgWorkTime,diff$OverTimeDays)
AvgWorkTime<-as.data.frame(AvgWorkTime)
colnames(AvgWorkTime)[1] <- "EmployeeID"
colnames(AvgWorkTime)[2] <- "AverageWorkTime"
colnames(AvgWorkTime)[3] <- "OverTimeDays"

#use setdiff to find out whether data is for all the employeeID
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID) 
setdiff(manager_survey_data$EmployeeID,general_data$EmployeeID) 
setdiff(general_data$EmployeeID,AvgWorkTime$EmployeeID) 
# since setdiff is 0 we know all data is for same employees only

#merge all the data frame to create a master file for analysis
attrition.data<-merge(employee_survey_data,manager_survey_data,by="EmployeeID",all=F)
attrition.data<-merge(attrition.data,general_data,by="EmployeeID",all=F)
attrition.data<-merge(attrition.data,AvgWorkTime,by="EmployeeID",all = F)
# now we have the master dataset


############################### Data Cleaning ##########################################

#Delete Columns With only one unique Value
z <- SingleUniqueValue.test(attrition.data)
attrition.data <- attrition.data[, - c(z)]


# Lets convert few numeric variable to categorical variable as per the dictionary 


attrition.data$Education <- revalue(as.factor(attrition.data$Education),
                                    c("1"='Below College',"2"='College',"3"='Bachelor',"4"='Master',"5"='Doctor'))

attrition.data$EnvironmentSatisfaction <- revalue(as.factor(attrition.data$EnvironmentSatisfaction),
                                                  c("1"='Low',"2"='Medium',"3"='High',"4"='Very High'))

attrition.data$JobInvolvement <- revalue(as.factor(attrition.data$JobInvolvement), 
                                         c("1"='Low',"2"='Medium',"3"='High',"4"='Very High'))

attrition.data$JobSatisfaction <- revalue(as.factor(attrition.data$JobSatisfaction),
                                          c("1"='Low',"2"='Medium',"3"='High',"4"='Very High'))

attrition.data$WorkLifeBalance <- revalue(as.factor(attrition.data$WorkLifeBalance),
                                          c("1"='Bad',"2"='Good',"3"='Better',"4"='Best'))

attrition.data$PerformanceRating <- revalue(as.factor(attrition.data$PerformanceRating),
                                            c("1"='Low',"2"='Good',"3"='Excellent',"4"='Outstanding'))


############################### exploratory data analysis ##########################
# Barcharts for categorical features with stacked attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

# Lets plot some categorical variables against attrition variable and try to explore some insights

plot_grid(ggplot(attrition.data, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(position='fill'), 
          ggplot(attrition.data, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          ggplot(attrition.data, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          ggplot(attrition.data, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          align = "h")   
# from the above plot it seems
#1. People with Low environment satisfaction are more prone to leave than any other
#2. People with Low job satisfaction are more prone to leave than any other
#3. People with Bad work life balance are more prone to leave than any other
#4. People with Low job involvement are more prone to leave than any other

plot_grid(ggplot(attrition.data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(position='fill'), 
          ggplot(attrition.data, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          ggplot(attrition.data, aes(x=factor(Department),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          ggplot(attrition.data, aes(x=factor(Education),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          align = "h")  

# from the above plot it seems
#1. There are no significant or major category of performance that shows attrition clearly
#2. People who travel Frequently are more prone to leave than any other
#3. People who are from 'Human Resources' department are more prone to leave than any other
#4. There are no significant or major category of education that shows attrition clearly

plot_grid(ggplot(attrition.data, aes(x=factor(EducationField),fill=Attrition))+ geom_bar(position='fill'), 
          ggplot(attrition.data, aes(x=factor(Gender),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          ggplot(attrition.data, aes(x=factor(JobRole),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          ggplot(attrition.data, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar(position='fill')+bar_theme1,
          align = "h")  
# from the above plot it seems
#1. People who studied 'Human Resources' are more prone to leave than any other
#2. There are no significant or major category of gender that shows attrition clearly
#3. People who are from 'Research Director' job role are more prone to leave than any other
#4. People who are single are more prone to leave than any other


#Summary of factor fields:
#Variables that have relationship with Attrition are:
#BusinessTravel, Department, EducationField, JobRole, MaritalStatus, JobSatisfaction 
#and WorkLifeBalance

# Histogram and Boxplots for numeric variables to find the median and outliers, if any
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(attrition.data, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# above plot reveals that Median age of employees is ~35

plot_grid(ggplot(attrition.data, aes(AverageWorkTime))+ geom_histogram(binwidth=1), 
          ggplot(attrition.data, aes(x="", y= AverageWorkTime))+ 
            geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)

# above plot reveals that Median work time for employees is ~7
# it also shows that there are outliers in this variable

plot_grid(ggplot(attrition.data, aes(OverTimeDays))+ geom_histogram(binwidth=1), 
          ggplot(attrition.data, aes(x="", y= OverTimeDays))+ 
            geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)

# above plot reveals that Median over time for employees is 2 days


plot_grid(ggplot(attrition.data, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+
            coord_flip()+box_theme,align = "v",ncol = 1)
# above plot reveals that distance from home for employees has an average of ~5 KM

plot_grid(ggplot(attrition.data, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# above plot reveals that monthly income for employees has median of 50000
# it also shows that there are outliers in this variable

plot_grid(ggplot(attrition.data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# above plot reveals that median salary hike for employees is 9%

plot_grid(ggplot(attrition.data, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+
            coord_flip()+box_theme,align = "v",ncol = 1)
# above plot reveals that average years for employees in company is 2
# it also shows that there are outliers in this variable

plot_grid(ggplot(attrition.data, aes(JobLevel))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=JobLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition.data, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# it  shows that there are outliers in this variable

plot_grid(ggplot(attrition.data, aes(StockOptionLevel))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# above plot reveals that stock option for employees in company is very less
# it also shows that there are outliers in this variable

plot_grid(ggplot(attrition.data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# above plot reveals that total working years for employees in company is around 9
# it also shows that there are outliers in this variable

plot_grid(ggplot(attrition.data, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# above plot reveals that total working years for employees in company is around 9
# it also shows that there are outliers in this variable

plot_grid(ggplot(attrition.data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# YearsSinceLastPromotion has outliers

plot_grid(ggplot(attrition.data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(attrition.data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# YearsWithCurrManager has outliers


############################ segmented analysis #####################################################

#From experience and logic, we know that males and females may value various  
#aspect of their career differently. Let's see if that data supports that logic

#Impact of gender on JobSatisfaction, WorkLifeBalance, EnvionmentSatisfaction
#JobInvolvement and BusinessTravel

ggplot(attrition.data,aes(x=Gender,fill=JobSatisfaction)) + geom_bar(position='fill')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) 
ggplot(attrition.data,aes(x=Gender,fill=WorkLifeBalance)) + geom_bar(position='fill')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) 
ggplot(attrition.data,aes(x=Gender,fill=EnvironmentSatisfaction)) + geom_bar(position='fill')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) 
ggplot(attrition.data,aes(x=Gender,fill=JobInvolvement)) + geom_bar(position='fill')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) 
ggplot(attrition.data,aes(x=Gender,fill=BusinessTravel)) + geom_bar(position='fill')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) 

#Gender does not have much impact on any of these variables. Females tend to rate
#WorkLifeBalance a little higher than men but very slightly so


#Impact of gender on Age vs. Attrition

ggplot(attrition.data, aes(x=Age,fill=Attrition))+
  geom_histogram(position = "fill")

pA<-ggplot(attrition.data[attrition.data$Gender=='Male',],aes(x=Age,fill=Attrition)) + 
  geom_histogram(position='fill') + ggtitle("Gender: Male")

pB<-ggplot(attrition.data[attrition.data$Gender=='Female',],aes(x=Age,fill=Attrition)) + 
  geom_histogram(position='fill') + ggtitle("Gender: Female")

require(gridExtra)

grid.arrange(pA,pB,nrow=2,ncol=1)

#We had noticed that with age attrition rate declines upto age 38 or so
#This decline is more rapid in females. It is relatively more gradual in males

#Impact of gender on overTimeDays vs. Attrition

ggplot(attrition.data, aes(x=OverTimeDays,fill=Attrition))+
  geom_histogram(position = "fill")

pA<-ggplot(attrition.data[attrition.data$Gender=='Male',],aes(x=OverTimeDays,fill=Attrition)) + 
  geom_histogram(position='fill') + ggtitle("Gender: Male")

pB<-ggplot(attrition.data[attrition.data$Gender=='Female',],aes(x=OverTimeDays,fill=Attrition)) + 
  geom_histogram(position='fill') + ggtitle("Gender: Female")


grid.arrange(pA,pB,nrow=2,ncol=1)

#It is evident that Females have a higher Attrition related to overTimeDays than Males


#Let's see if variables impacting Attrition vary among Department

#Impact of Department on overTimeDays
ggplot(attrition.data, aes(x=OverTimeDays,fill=Attrition))+
  geom_histogram(position = "fill")

pA<-ggplot(attrition.data[attrition.data$Department=='Human Resources',],aes(x=OverTimeDays,fill=Attrition)) + 
  geom_histogram(position='fill') + ggtitle("Department: Human Resources")

pB<-ggplot(attrition.data[attrition.data$Department=='Research & Development',],aes(x=OverTimeDays,fill=Attrition)) + 
  geom_histogram(position='fill') + ggtitle("Department: Research & Development")

pC<-ggplot(attrition.data[attrition.data$Department=='Sales',],aes(x=OverTimeDays,fill=Attrition)) + 
  geom_histogram(position='fill') + ggtitle("Department: Sales")

grid.arrange(pA,pB,pC,nrow=3,ncol=1)

#It seems all the departments are impacted by OverTimeDays.
#HR seems to have a more pronounced impact of OverTimeDays on Attrition rate
#It seems as OverTimeDays reach above 130 days in a year, attition rate seems to jump up


#Some more derived variables

attrition.data$NormYrsSincePromotion <- attrition.data$YearsAtCompany - attrition.data$YearsSinceLastPromotion

#Assuming a 5 day workweek and 4 weeks in a month, dividing the monthly income by the number of working days in
#a month (20) and the actual working hours, will give us the actual hourly income of each employee.
attrition.data$NormIncomeToWorktime <- attrition.data$MonthlyIncome/(attrition.data$AverageWorkTime*20)

plot_grid(ggplot(attrition.data, aes(NormYrsSincePromotion,fill=Attrition))+ geom_histogram(),
          ggplot(attrition.data, aes(NormYrsSincePromotion,fill=Attrition))+ geom_histogram(position='fill'),
          ggplot(attrition.data, aes(NormIncomeToWorktime,fill=Attrition))+ geom_histogram(),
          ggplot(attrition.data, aes(NormIncomeToWorktime,fill=Attrition))+ geom_histogram(position='fill'),
          align = "h")

#It looks like Attrition rate is inversely propotional to both the above derived variables.

################################ Data Preparation ###############################

# Outlier treatment 
#removing outliers based on above boxplots and findings
outliers_values <- boxplot.stats(attrition.data$YearsAtCompany)$out
outliers <- attrition.data[!attrition.data$YearsAtCompany %in% outliers_values,]
attrition_data <- outliers

outliers_values <- boxplot.stats(attrition_data$AverageWorkTime)$out
outliers <- attrition_data[ !attrition_data$AverageWorkTime %in% outliers_values, ]
attrition_data <- outliers

outliers_values <- boxplot.stats(attrition_data$OverTimeDays)$out
outliers <- attrition_data[!attrition_data$OverTimeDays %in% outliers_values,]
attrition_data <- outliers

outliers_values <- boxplot.stats(attrition_data$StockOptionLevel)$out
outliers <- attrition_data[ !attrition_data$StockOptionLevel %in% outliers_values, ]
attrition_data <- outliers

outliers_values <- boxplot.stats(attrition_data$TotalWorkingYears)$out
outliers <- attrition_data[ !attrition_data$TotalWorkingYears %in% outliers_values, ]
attrition_data <- outliers

outliers_values <- boxplot.stats(attrition_data$TrainingTimesLastYear)$out
outliers <- attrition_data[ !attrition_data$TrainingTimesLastYear %in% outliers_values, ]
attrition_data <- outliers

outliers_values <- boxplot.stats(attrition_data$YearsSinceLastPromotion)$out
outliers <- attrition_data[ !attrition_data$YearsSinceLastPromotion %in% outliers_values, ]
attrition_data <- outliers

outliers_values <- boxplot.stats(attrition_data$YearsWithCurrManager)$out
outliers <- attrition_data[ !attrition_data$YearsWithCurrManager %in% outliers_values, ]
attrition_data <- outliers

# Missing value treatment
# Lets find out the number of NA's in each column
sapply(attrition_data, function(x) sum(is.na(x)))

# Lets check the percent of NA's by sum(is.na(attrition_data))/ nrow(attrition_data)
# it seems only 2% of NA's are present. It is so less that we can remove it
attrition_data <- attrition_data[!is.na(attrition_data$EnvironmentSatisfaction),]
attrition_data <- attrition_data[!is.na(attrition_data$JobSatisfaction),]
attrition_data <- attrition_data[!is.na(attrition_data$WorkLifeBalance),]
attrition_data <- attrition_data[!is.na(attrition_data$NumCompaniesWorked),]
attrition_data <- attrition_data[!is.na(attrition_data$TotalWorkingYears),]

#Lets count the number for NAs again
sapply(attrition_data, function(x) sum(is.na(x))) 
# we have no NAs

# Now we have no NAs and no Outliers

############################# Feature standardisation ###################################3
#scaling continuos variables

attrition_data$Age<- scale(attrition_data$Age) 
attrition_data$DistanceFromHome<- scale(attrition_data$DistanceFromHome) 
attrition_data$JobLevel<- scale(attrition_data$JobLevel) 
attrition_data$MonthlyIncome<- scale(attrition_data$MonthlyIncome) 
attrition_data$NumCompaniesWorked<- scale(attrition_data$NumCompaniesWorked) 
attrition_data$PercentSalaryHike<- scale(attrition_data$PercentSalaryHike) 
attrition_data$StockOptionLevel<- scale(attrition_data$StockOptionLevel) 
attrition_data$TotalWorkingYears<- scale(attrition_data$TotalWorkingYears) 
attrition_data$TrainingTimesLastYear<- scale(attrition_data$TrainingTimesLastYear) 
attrition_data$YearsAtCompany<- scale(attrition_data$YearsAtCompany) 
attrition_data$YearsSinceLastPromotion<- scale(attrition_data$YearsSinceLastPromotion) 
attrition_data$YearsWithCurrManager<- scale(attrition_data$YearsWithCurrManager) 
attrition_data$AverageWorkTime<- scale(attrition_data$AverageWorkTime) 
attrition_data$OverTimeDays<- scale(attrition_data$OverTimeDays) 

# convert variable Attrition from No/Yes  to  0/1 
attrition_data$Attrition<- ifelse(attrition_data$Attrition =="Yes",1,0)

# Lets check the % of attrition
attrition_percentage <- sum(attrition_data$Attrition)/nrow(attrition_data)
attrition_percentage # 17.57% attrition rate. 

# convert categorical variables into factor and then create dummy variables
attrition_factor<- attrition_data[,c(2,3,4,5,6,9,10,12,13,14,16,17)]
attrition_fact<- data.frame(sapply(attrition_factor, function(x) factor(x)))

# create dummy variables for factor variables

dummies <- data.frame("")

f = function(x){
  dummy <- data.frame(model.matrix(~x,data =attrition_fact))
  dummy <- dummy[,-1]
}

dummies <- cbind(dummies[,-1], sapply(attrition_factor,f))

# Merged and final dataset
attrition<- cbind(attrition_data[,c(7,8,11,15,18,19,20,21,22,23,24,25,26,27,28)],dummies) 

################################### Model Building #############################################

# split the data between train and test
set.seed(100)
indices = sample.split(attrition$Attrition, SplitRatio = 0.7)
train = attrition[indices,]
test = attrition[!(indices),]

# Lets use Logistic Regression to build the model 

#Initial model with all variables
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

library("MASS")

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

# Let's execute this model here, 
model_2<- stepAIC(model_1, direction="both")
# Let us look at the summary of the model
summary(model_2)

## Let us check for multicollinearity 
# If the VIF is above 2, you would remove the variables if they are statistically insignificant

library(car)
vif(model_2)
# it seems we have all VIF less than 2. Lets check the P value and decide which variable to remove

#AIC is 1183.6
#VIF for Department.xSales is 4.47 and for Department.xResearch&Development is 4.2
#Eliminiate Department.xSales
model_3<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Education.xCollege + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_3)
sort(vif(model_3)) 
#VIF for Department.xResearch&Development has now reduced to less than 2
#VIF for WorkLifeBalance.xGood is 3.7 and WorkLifeBalance.xBetter is 4.2. 
#Let's eliminate WorkLifeBalance.xGood even though VIF is less because P value is higher than other


model_4<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Education.xCollege + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_4)
vif(model_4)


#VIF for  MaritalStatus.xSingle is 2.34 and MaritalStatus.xMarried is 2.32 
#Let's eliminate MaritalStatus.xMarried even though VIF is less because P value is higher than other
model_5<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Education.xCollege + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_5) 
sort(vif(model_5))

# BusinessTravel.xTravel_Rarely has VIF of 3.03 and BusinessTravel.xTravel_Frequently has VIF of 3.14
# Eliminate BusinessTravel.xTravel_Rarely as this has higher P value than other
model_6<-  glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                 OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                 WorkLifeBalance.xBetter + WorkLifeBalance.xBest + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Education.xCollege + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle, family = "binomial", 
               data = train)
summary(model_6)
sort(vif(model_6))
# all the VIF are now less than 2, lets remove variable now using the P value

#Excluding Department.xResearch...Development as this has higher p value
model_7<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + BusinessTravel.xTravel_Frequently + 
                Education.xCollege + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_7) 

#Excluding WorkLifeBalance.xBest as this has high P value
model_8<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                WorkLifeBalance.xBetter + BusinessTravel.xTravel_Frequently + 
                Education.xCollege + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_8) 


#Excluding Education.xCollege as this has higher P value
model_9<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
               PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
               OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
               WorkLifeBalance.xBetter + BusinessTravel.xTravel_Frequently + 
               EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle, family = "binomial", 
             data = train)

summary(model_9) 

#Excluding WorkLifeBalance.xBetter as this has higher p value
model_10<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_10) 

#excluding EducationField.xMedical as this has higher p value
model_11<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + 
                EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)


summary(model_11)

#Excluding EducationField.xTechnical.Degree as this has higher p value
model_12<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + EducationField.xOther +  
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)


summary(model_12)

#exlcuding JobRole.xResearch.Scientist as this has higher p value
model_13<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + EducationField.xOther +  
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_13)


#exlcuding JobRole.xLaboratory.Technician as this has higher p value
model_14<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + EducationField.xOther +  
                JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)


summary(model_14)

#exlcuding EducationField.xMarketing as this has higher p value
model_16<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + EducationField.xOther +  
                JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_16)

#exlcuding JobRole.xSales.Executive as this has higher p value
model_17<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + EducationField.xOther +  
                JobRole.xResearch.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_17)

# excluding DistanceFromHome as this higher p value
model_18<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + EducationField.xOther +  
                JobRole.xResearch.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_18)


# excluding PercentSalaryHike as this higher p value
model_19<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + EducationField.xOther +  
                JobRole.xResearch.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_19)




# excluding Age as this higher p value
model_20<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently + EducationField.xOther +  
                JobRole.xResearch.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)


summary(model_20)


# excluding EducationField.xOther as this higher p value
model_21<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently +   
                JobRole.xResearch.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)


summary(model_21)

# excluding JobRole.xResearch.Director as this higher p value
model_22<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + YearsWithCurrManager + 
                OverTimeDays + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High +  
                BusinessTravel.xTravel_Frequently +   
                MaritalStatus.xSingle, family = "binomial", 
              data = train)


summary(model_22)

# 8 significant variables in the model and all have ***
final_model<- model_22

# we were able to idenfify variables that are most important and needs to be addressed right away.

###################### Summary of the variables/factors ####################################

# These are the factors/features/variables which  is quite significant in predicting attrition rate
#  NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + OverTimeDays +  
#  EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High
#  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle  

###############################################################################################################

######################## Model Evaluation ############################

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])
# -2 for excluding Attrition variable

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_pred_attrition,test_actual_attrition)
confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")

# Let's use the probability cutoff of 40%. for test_pred
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")
test_conf
# After using the prob of 40% as cut off, statistics have changed


#########################################################################################
# Let's Choose the cutoff value. 
# Finding optimal probability cuttoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
# Lets create a sequence vector of several proabilites
s = seq(.01,.80,length=100)
# Lets create a matrix to store all the sensitivity, specificity and accuracy 
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# Lets create the plot and try to see the intersection for best cutoff value
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.5,.90,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Assumption : We will take that cutoff where the difference in sensitivity and specificity is < 0.01
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff  # 0.1616162

# Let's choose a cutoff value as found for final model
test_cutoff_attrition<- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc # 0.7301587 
sens # 0.7368421
spec # 0.7287319

# Findings and model's discriminative power

# with this final model with cutoff of 0.1616162, we can see that our
# accuracy is 0.7301587 and sensitivity is 0.7368421 and specificity is 0.7287319
# Our model is quite good providing all the 11 driver variables and with high discriminative power

###########################################################################################

# Lets see some more measures for assessing the discriminative power of the final model
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# KS found to be 0.465574 which is more than industry standard of 40%
# Usually KS statistic gives an indicator of where our model lie between the 
# two models (perfect and random)

###############################################################################################
# Lets see the Lift & Gain chart 

require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

plot_grid(ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)

##################################################################################################


