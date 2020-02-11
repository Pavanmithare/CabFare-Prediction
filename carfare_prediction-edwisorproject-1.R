# Clearing Environ ment
rm(list = ls())

#insatll Required packages for the project
x = c("lubridate","dplyr","mice","VIM","Amelia","missForest",
      "e1071","ggplot2","corrplot","rpart","geosphere","corrgram",
      "DMwR","purrr","geosphere","rlist","PerformanceAnalytics",
      "xgboost","car","caret","randomForest","mlbench","unbalanced",
      "C50","dummies","e1071","Information","MASS","gbm","ROSE",'sampling',
      'DataCombine','inTrees',"scales","psych","gplots")
#install.packages("x")
# Check whether Required libraries are loaded into environment:
lapply(x, require, character.only = TRUE)


rm(x)
#set working directory
getwd()
setwd("C:/Users/Rahul/Documents")
getwd()
#load the training data
train_cab <- read.csv("train_cab.csv")
#lets preview the data 
head(train_cab)
#load the test data
test <- read.csv("test.csv")
#lets preview the test data
head(test)

##########(EDA Preprocing)explore the data#############

#check class of the data
class(train_cab)

#check the dimensions of the data (no of rows and no of columns)
dim(train_cab)

#check the names of the dataset
names(train_cab)

#############Overview of the traing and test data#############
#lets check for data types of train data
sapply(train_cab,class)
str(train_cab)

#in train data set observed that fare_amount and pickup_datetime variables are of Factor type
#and pasenger varibale of numeric type
#so Need to convert fare_amount datatype to numeric type and pickup_datetime data type to dataTime format
#passenger data type to integer data type

#lets check for the data type of test data
sapply(test,class)
str(test)

#in test data observed that pickup_datatime variable is of Factor type
#so need to convert it to datatime format

#Lets understand basic statistics of each (numeric and non numeric) variables in train data
summary(train_cab, digits = 4, maxsum = 6)


#Response variable or Target variable--Fare_amount
str(train_cab$fare_amount)


############Data type conversion#############
#convert fare_amount data type form Factor to Numeric type
class(train_cab$fare_amount)#beofe converting to numeric type
train_cab$fare_amount = as.numeric(as.character(train_cab$fare_amount), coercion=TRUE)
# because in our dataset data time variable has na in one of the observation need to handle that first
class(train_cab$fare_amount)#after converting data

#convert passenger_count data type from numeric to integer
class(train_cab$passenger_count)#before converting 
train_cab$passenger_count = as.integer(as.character(train_cab$passenger_count))
class(train_cab$passenger_count)#after convertig

#converting pickup_datetime data type from factor to datetime
train_cab$pickup_datetime <- as.POSIXct(strptime(train_cab$pickup_datetime,"%Y-%m-%d %H:%M:%S"))
str(train_cab$pickup_datetime)
head(train_cab)
summary(train_cab)#one observation is not in correct form in pickup_datetime-we need to delet


#check observations which are not in correct form in pickup_datatime variable
train_cab[is.na(strptime(train_cab$pickup_datetime,format="%Y-%m-%d %H:%M:s")),]

#remove obsveration which are not correct date time format-1 obsvervation found
sum(is.na(train_cab$pickup_datetime)) # in datatime variable wee have 0ne NA observation need to remove

train_cab <- train_cab[-c(1328),]
dim(train_cab)    
summary(train_cab)


#extract seven new variables form pickup_datetime variable from train data
#viz,year,month,date,dayofweek,hour,minutes,seconds.
#after extracting all relevant information from pickup_datetime variable-just delet
library(lubridate)
train_cab <- train_cab %>% mutate(year = year(pickup_datetime),month = month(pickup_datetime),date = day(pickup_datetime),dayofweek = wday(pickup_datetime),hour = hour(pickup_datetime),minutes = minute(pickup_datetime))
train_cab$seconds <- substr(as.factor(train_cab$pickup_datetime),18,19)

#converting seconds varibales to integer type because Factor or character type are not supported in ML algorithms.
train_cab$seconds <- as.integer(train_cab$seconds)

#Removing pickup_datetime variabe from train data set
train_cab$pickup_datetime <- NULL
train_cab <- train_cab[,c(2:13,1)]
head(train_cab)
str(train_cab)


##############pickup_datetime of test data test###############

#convert pickup_datetime data type to Factor to datetime
library(lubridate)
test$pickup_datetime <-as.POSIXct(strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S"))
str(test$pickup_datetime)

#extract six new variables form pickup_datetime variable from test data
#viz,year,month,date,dayofweek,hour,minutes,seconds.
#after extracting all relevant information from pickup_datetime variable-just delet
test <- test %>% mutate(year = year(pickup_datetime),month = month(pickup_datetime),date = day(pickup_datetime),dayofweek = wday(pickup_datetime),hour = hour(pickup_datetime),minutes = minute(pickup_datetime))
test$seconds <- substr(as.factor(test$pickup_datetime),18,19)

#converting seconds varibales to integer type because Factor or character type are not supported in ML algorithms.
test$seconds <- as.integer(test$seconds)

#Removing pickup_datetime variabe from train data set
test$pickup_datetime <- NULL
head(train_cab)
str(train_cab)


#Let's understand basic statistics of fare_amount variable after  data type convertion in train dataset
summary(train_cab,digits = 4, maxsum = 6)
str(train_cab)

#Let's understand basic statistics of fare_amount variable after  data type convertion in test dataset
summary(test,digits = 4, maxsum = 6)
str(test)


###################Missing value Analysis#####################
#Total number of missing values present in train data set
sum(is.na(train_cab))

#Total number of Missing values present in test data set
sum(is.na(test))


############Let's calculate missing value percentage in train data set############
#convert table to data frame
missing_val = data.frame(apply(train_cab,2,function(x){sum(is.na(x))}))

#Rename column 1 in missing_val dataframe
names(missing_val)[1] = "Missing_values"

#Let's calculate missing value percentage
missing_val = cbind(missing_val,Missing_values_percentage=(missing_val$Missing_values/nrow(train_cab))*100)

#sort the missing percentage in descending order
#-ve sign used for descending order
missing_val = missing_val[order(-missing_val$Missing_values_percentage),1]
missing_val


###############Let's use mice package to check missing values.###################
#mice package has a function known as md.pattern()
#It returns a tabular form of missing values present in each variable of train data set(for numerical variables)
md.pattern(train_cab,rotate.names = TRUE)

#Let's visualise missing values using VIM package
mice_plot <- aggr(train_cab, col=c('pink','brown'), numbers=TRUE,cex.axis=6, gap=1,lables=names(train_cab), sortvars=TRUE, cex.numbers=0.6, ylab=c("Missing data","pattern"))


#############Missing value imputation using Mice##################
colnames(train_cab)
#Remove categorical variable if any:
#train_cab.mis <- subset(train_cab, select = -c(dayofweek))
train_cab.mis <- train_cab #as in our dataset all are int and num values are there
summary(train_cab.mis)


#here is an explanation of parameters used :(note:it will take 1-2 min to excute the code)
#m - refers to 5 imputed data sets
#maxit - refers to number of iterations taken to impute missing values
#method - refers to method used in imputation
imputed_Data <- mice(train_cab.mis,m=5, maxit = 50, method = 'pmm',seed = 500)
summary(imputed_Data)

#check imputed values
#imputed_data$imp$passenger_count
head(imputed_Data$imp$fare_amount,25)

#since there 5 imputed data sets, we can select any using complete()function
#get complete data (2nd out of 5)
miceimputed_train_cab <- complete(imputed_Data)
#miceimputed_train_cab

# MICE model for missing value imputation
dim(miceimputed_train_cab)
dim(train_cab.mis)
train_cab <- miceimputed_train_cab
head(train_cab)


#store these missing values present in each varibale in to data frame
missing_val = data.frame(apply(train_cab,2, function(x){sum(is.na(x))}))

#Rename column 1 in missing_val dataframe
names(missing_val)[1] = "Missing_values"

# Let's calculate percentage of missing values
missing_val = cbind(missing_val,Missing_values_percentage = (missing_val$Missing_values/nrow(train_cab))*100)
# sort the missing percentage in descending order
missing_val = missing_val[order(-missing_val$Missing_values_percentage),]
missing_val
 
#check train data set after missing value imputation
dim(train_cab)
head(train_cab)
#Let's summary after new feature creation
summary(train_cab)
summary(test)

##################Outlier Analysis###############################
summary(train_cab)
# Check whether the fare amount varibale has any zero or nagative values
sum(with(train_cab,train_cab$fare_amount<1))

#Remove the rows where fare amount varible has zero or nagative values
#train_cab1<-train_cab[train_cab$fare_amount>1,]
train_cab <- filter(train_cab,fare_amount>1)
dim(train_cab)

#In a cab maximum allowable passengers is 6 considered XUV500 vehicle for cab service (6 passenger +1 Driver)
sum(with(train_cab,train_cab$passenger_count<1))
sum(with(train_cab,train_cab$passenger_count>6))

#In a cab maximum allowable passengers is 6 considered XUV500 vehicle for cab service (6 passenger +1 Driver)
sum(with(test,test$passenger_count<1))
sum(with(test,test$passenger_count>6))

#Remove the rows where passenger count has <1 and >6.
train_cab <- filter(train_cab,passenger_count >=1)
train_cab <- filter(train_cab,passenger_count <=6)
dim(train_cab)

# As we see that Passenger Count variable has fraction values,
# but in reality it should be whole number hence Need to round them to integer.
train_cab$passenger_count <- round(train_cab$passenger_count)
train_cab$passenger_count <- as.integer(train_cab$passenger_count)

# Let's Check test dataset for Passenger Count variable for fraction values,if yes Need to round them to integer.
test$passenger_count <- round(test$passenger_count)
test$passenger_count <- as.integer(test$passenger_count)
str(train_cab)
str(test)

#get all column names
colnames(train_cab)
colnames(test)


#Boxplots distribution and outlier check for traing data set
#selecting only numeric variables
numeric_index <- sapply(train_cab,is.numeric)
numeric_index
numeric_data <- train_cab[,numeric_index]
cnames.train <- colnames(numeric_data)
cnames.train

#Boxplots distribution and outlier check for test data set
#selecting only numeric variables
numeric_index <- sapply(test,is.numeric)
numeric_index
numeric_data <- test[,numeric_index]
cnames.test <- colnames(numeric_data)
cnames.test

print(dim(train_cab))
print(dim(test))
str(train_cab)


# Boxplot for all continous variables in train data set
for (i in 1:length(cnames.train))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames.train[i]), data =train_cab)+stat_boxplot(geom = "errorbar", width = .4) +geom_boxplot(outlier.colour="red", fill = "LightGreen" ,outlier.shape=18,outlier.size=1, notch=FALSE) +
           #theme(legend.position="bottom")+
           #labs(y=cnames.train[i])+
           ggtitle(paste(" ",cnames.train[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,gn7,gn8,gn9,gn10,gn11,gn12,ncol=3)

# Boxplot for all continous varaibles in test dataset
  for (i in 1:length(cnames.test))
  {
    assign(paste0("gn",i), ggplot(aes_string(y = cnames.test[i]), data =test)+stat_boxplot(geom = "errorbar", width = .4) +
             geom_boxplot(outlier.colour="red", fill = "Violet" ,outlier.shape=18,
                          outlier.size=1, notch=FALSE) +
             theme(legend.position="bottom")+
             labs(y=cnames.test[i])+
             ggtitle(paste(" ",cnames.test[i])))
  }
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,gn7,gn8,gn9,gn10,gn11,gn12,ncol=3)


# Check observations having pickup longitute and pickup latitute out the range in train dataset.
#Longitude range----(-180 to 180)
#Latitude range----(-90 to 90)

train_cab[train_cab$pickup_longitude < -180,]
train_cab[train_cab$pickup_longitude > 180,]
train_cab[train_cab$pickup_latitude < -90,]
train_cab[train_cab$pickup_latitude > 90,]

# Check observations having dropoff longitute and dropoff latitute out the range in train dataset.
train_cab[train_cab$dropoff_longitude < -180,]
train_cab[train_cab$dropoff_longitude > 180,]
train_cab[train_cab$dropoff_latitude < -90,]
train_cab[train_cab$dropoff_latitude > 90,]

# Dropping the observations which are outof range in train dataset:
train_cab <- filter (train_cab,pickup_longitude > -180)
train_cab <- filter (train_cab,pickup_longitude < 180)
train_cab <- filter (train_cab,pickup_latitude > -90)
train_cab <- filter (train_cab,pickup_latitude < 90)
dim(train_cab)

train_cab <- filter (train_cab,dropoff_longitude > -180)
train_cab <- filter (train_cab,dropoff_longitude < 180)
train_cab <- filter (train_cab,dropoff_latitude > -90)
train_cab <- filter (train_cab,dropoff_latitude < 90)
dim(train_cab)


# Check observations having pickup longitute and pickup latitute out the range in test dataset.
#Longitude range----(-180 to 180)
#Latitude range----(-90 to 90)

test[test$pickup_longitude < -180,]
test[test$pickup_longitude > 180,]
test[test$pickup_latitude < -90,]
test[test$pickup_latitude > 90,]

# Check observations having dropoff longitute and dropoff latitute out the range in test dataset.
test[test$dropoff_longitude < -180,]
test[test$dropoff_longitude > 180,]
test[test$dropoff_latitude < -90,]
test[test$dropoff_latitude > 90,]

# Dropping the observations which are outof range in test dataset:
test<- filter (test,pickup_longitude > -180)
test<- filter (test,pickup_longitude < 180)
test<- filter (test,pickup_latitude > -90)
test<- filter (test,pickup_latitude < 90)
dim(test)

test<- filter (test,dropoff_longitude > -180)
test<- filter (test,dropoff_longitude < 180)
test<- filter (test,dropoff_latitude > -90)
test<- filter (test,dropoff_latitude < 90)
dim(test)

train_cab2 = train_cab
#train_cab = train_cab2
dim(train_cab2)


# Get all column names
cnamestrain <- which( sapply(train_cab, is.numeric ))
#cnamestrain <- cnamestrain[-6]
cnamestrain
cnames=cnamestrain
summary(train_cab$passenger_count)

cnamestest <- which( sapply(test, is.numeric ))
#cnamestest <- cnamestest[-5]
cnamestest


#We have different methods like imputation and capping methods to remove outliers here we
#used capping method:
#  Capping with Upper limit /Lower limit Value in case of outliers
#Q1 <- first quartile(25%)
#Q3 <- third quartile(75%)
#IQR <- Inter quartile range = Q3-Q1
#UL <- Q3 +1.5IQR Uppler limit/upper fence #### LL <- Q1 - 1.5IQR Lower limit /lower fence

# Exclude 'passenger_count' variable in train dataset reason need to set upper limit at 6 because maximum allowable count are 6 passengers.
for(i in 1:length(cnamestrain)){
  if(cnamestrain[i] != cnamestrain[5]){
    Q1 <- quantile(train_cab[,cnamestrain[i]],0.25)
    #print(Q1)
    Q3 <- quantile(train_cab[,cnamestrain[i]],0.75)
    #print(Q3)
    UL <- Q3 + (1.5*IQR(train_cab[,cnamestrain[i]]))
    #print(UL)
    LL <- Q1 -(1.5*IQR(train_cab[,cnamestrain[i]]))
    #print(LL)
    train_cab[train_cab[,cnamestrain[i]]<LL,cnamestrain[[i]]]<- LL
    train_cab[train_cab[,cnamestrain[i]]>UL,cnamestrain[[i]]]<- UL
  }
}
#print(Q1)
#print(Q3)
#print(UL)
#print(LL)

# Exclude 'passenger_count' variable in test dataset also to set upper limit at 6 because maximum allowable count are 6 passengers.
for (i in 1:length(cnamestest)){
  if(cnamestest[i] != cnamestest[5]){
    Q1 <- quantile(test[,cnamestest[i]],0.25)
    #print(Q1)
    Q3 <- quantile(test[,cnamestest[i]],0.75)
    #print(Q3)
    UL <- Q3 + (1.5*IQR(test[,cnamestest[i]]))
    #print(UL)
    LL <- Q1 -(1.5*IQR(test[,cnamestest[i]]))
    #print(LL)
    test[test[,cnamestest[i]]<LL,cnamestest[[i]]]<- LL
    test[test[,cnamestest[i]]>UL,cnamestest[[i]]]<- UL
  }
}
summary(train_cab)

# passenger_count variable of train dataset
summary(train_cab$passenger_count)

Q1 <- quantile(train_cab$passenger_count,0.25)
Q3 <- quantile(train_cab$passenger_count,0.75)

UL <- round(Q3 + (1.5*IQR(train_cab$passenger_count))) # 4
LL <- round(Q1 - (1.5*IQR(train_cab$passenger_count))) #0

# practically maximum 6 passenger can travel in a cab
train_cab[train_cab$passenger_count < LL,"passenger_count"] <-LL
train_cab[train_cab$passenger_count > 6,"passenger_count"] <-UL

# Let's check After setting the upper and lower limits to passenger_count variable of train data
summary(train_cab$passenger_count)


# passenger_count variable of test dataset
summary(test$passenger_count)
Q1 <- quantile(test$passenger_count,0.25)
Q3 <- quantile(test$passenger_count,0.75)
UL <- round(Q3 + (1.5*IQR(test$passenger_count))) # 4
LL <- round(Q1 - (1.5*IQR(test$passenger_count))) #0
# practically maximum 6 passenger can travel in a cab
test[test$passenger_count < LL,"passenger_count"] <-LL
test[test$passenger_count > 6,"passenger_count"] <-UL

# Let's check After setting the upper and lower limits to passenger_count variable of test data
summary(test$passenger_count)


# Lets visualize Boxplots again after outliers removal:
# colnames(train_cab)
cnames <- colnames(train_cab)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = train_cab)+
           stat_boxplot(geom = "errorbar", width = 1) +
           geom_boxplot(outlier.colour="red", fill = "green" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           #theme(legend.position="left")+
           theme(plot.title = element_text(hjust = 0, face = "bold"))+
           labs(y=cnames[i], title = cnames[i])+
           ggtitle(paste("",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,gn7,gn8,gn9,gn10,gn11,gn12,gn13,ncol=3)


# Boxplot for all continous varaibles in test dataset
cnames.test <- colnames(test)
for (i in 1:length(cnamestest))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames.test[i]), data = test)+
           stat_boxplot(geom = "errorbar", width = .4) +
           geom_boxplot(outlier.colour="red", fill = "Violet" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames.test[i])+
           ggtitle(paste(" ",cnames.test[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,gn7,gn8,gn9,gn10,gn11,gn12,ncol=3)
summary(train_cab)

# Lets check dimensions of data after outlier removal
dim(train_cab)
dim (test)



#As our both train and test data have longitude and latitude details can we extract distance out of it? Answer is YES
#Now, let's create distance using Haversine Formula. Calculates the geodesic distance between
#two points specified by Radian latitude/longitude using the Haversine formula
library(purrr)
library(geosphere)
library(rlist)
get_geo_distance = function(long1, lat1, long2, lat2) {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  #if (units == "km") {
  distance = distance_m / 1000.0;
  distance
}

# Applying distance formula for train data
for(i in (1:nrow(train_cab)))
{
  train_cab$distance[i]=get_geo_distance(train_cab$pickup_longitude[i],train_cab$pickup_latitude[i],
  train_cab$dropoff_longitude[i],train_cab$dropoff_latitude[i])
}

# Applying distance formula for test data (Distance calculation is based on manhautten distance calculation)
for(i in (1:nrow(test)))
{
  test$distance[i]=get_geo_distance(test$pickup_longitude[i],test$pickup_latitude[i],
  test$dropoff_longitude[i],test$dropoff_latitude[i])
}

# Lets check train data after distance variable creation
train_cab <- train_cab[,c(1:12,14,13)]
head(train_cab)

# Lets check test data after distance variable creation
head(test)

dim(train_cab)
dim(test)


# Lets check whether distance variable of train dataset has any outlier using boxplot
boxplot(train_cab$distance,
        main = "Box Plot of distance variable of Train dataset",
        xlab = "Distance Variable",
        ylab = "Frequency",
        col = "orange",
        border = "brown",
        horizontal = FALSE )

# Let's check whether distance variable of test dataset has any outlier using boxplot
boxplot(test$distance,
        main = "Box Plot of distance variable of Test dataset",
        xlab = "Distance Variable",
        ylab = "Frequency",
        col = "Green",
        border = "brown",
        horizontal = FALSE )

# Lets check summary of distance variable of Train & Test dataset
summary(train_cab$distance)
summary(test$distance)

# Let's check distance variable of train data for less than 1 values.
length(train_cab$distance[train_cab$distance < 1])#values less than 1--2986

# Let's check distance variable of test data for less than 1 values.
length(test$distance[test$distance < 1])#values less than 1--1549


#As distance variable has 2986 values in train dataset and 1549 values in test dataset less-than 1.
#Practically distance cannot be Zero or lessthan 1. So it is better to impute it with average value
#so that variable will be normally distributed.
train_cab$distance[train_cab$distance < 1] <- mean(train_cab$distance)
test$distance[test$distance < 1] <- mean(test$distance)
# Final train data and test data dimentions after all preprocessing
dim( train_cab)
dim( test)

# Let's use capping method to set upper & lower limits to distance variable of train dataset
summary(train_cab$distance)
Q1 <- quantile(train_cab$distance,0.25)
Q3 <- quantile(train_cab$distance,0.75)
UL <- round(Q3 + (1.5*IQR(train_cab$distance)))
LL <- round(Q1 - (1.5*IQR(train_cab$distance)))
train_cab[train_cab$distance < LL,"distance"] <- LL
train_cab[train_cab$distance > UL,"distance"] <- UL

# Let's use capping method to set upper & lower limits to distance variable of test dataset
summary(test$distance)
Q1 <- quantile(test$distance,0.25)
Q3 <- quantile(test$distance,0.75)
UL <- round(Q3 + (1.5*IQR(test$distance)))
LL <- round(Q1 - (1.5*IQR(test$distance)))
test[test$distance < LL,"distance"] <-LL
test[test$distance > UL,"distance"] <-UL

# Let's check summary After setting upper and Lower limits to distance variable of train dataset.
summary(train_cab$distance)
# Lets visuavalise BOXPLOT of distance variable of train dataset after removal of outliers
boxplot(train_cab$distance,
        main = "Box Plot: Distance variable (Train data) After Outlier Removal",
        xlab = "Distance Variable",
        ylab = "Frequency",
        col = "orange",
        border = "brown",
        horizontal = FALSE )

# Let's check summary After setting upper and Lower limits to distance variable of test dataset.
summary(test$distance)
# Lets visuavalise BOXPLOT of distance variable of test dataset after removal of outliers:
  boxplot(test$distance,
          main = "Box Plot: Distance variable (Test data) After Outlier Removal",
          xlab = "Distance Variable",
          ylab = "Frequency",
          col = "Green",
          border = "brown",
          horizontal = FALSE )
  
  
## Multi-Variate Analysis, Univariate Analysis and Bi-Variate Analysis of Train dataset
##########------MULTIVARIATE ANALYSIS------##########
#A rule of thumb for interpreting the variance in???ation factor:
#VIF = 1 ( Not correlated ) ;
#1 < VIF < 5 ( Moderately correlated );
#  VIF >=5 ( Highly correlated )
  
# Load the data
data <- train_cab
# Split the data into training and test set
training.samples <- train_cab$fare_amount %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- train_cab[training.samples, ]
test.data <- train_cab[-training.samples, ]
# Build the model
model1 <- lm(fare_amount ~., data = train.data)
# Make predictions
predictions <- model1 %>% predict(test.data)
# Model performance
data.frame(RMSE = RMSE(predictions, test.data$fare_amount),
    R2 = R2(predictions, test.data$fare_amount))

car::vif(model1)


####All variables are VIF Values less than 2 , Hence our data is safe. In case of high VIF we will
###remove those observations from our dataset for further analysis
# Let's cross check by Building a model excluding the high VIF variable pickup_latitude variable
model2 <- lm(fare_amount ~. -pickup_latitude, data = train.data)
# Make predictions
predictions <- model2 %>% predict(test.data)
# Model performance
data.frame(RMSE = RMSE(predictions, test.data$fare_amount),
  R2 = R2(predictions, test.data$fare_amount))
#  Removing the pickup_latitude variable does not affect very much the model performance metrics.

########### ------UNIVARIATE ANALYSIS------#############
#Univariate Analysis : Displays the statistic details or descriptive statistics of each variable
#Histogram for (Numeric) Continuous variables to check distribution of each variable
## Exploratory Analysis with visualizations after data cleaning
# check for numeric variables in train data set:
x = sapply(train_cab, is.numeric)
x


#####Univariate Analysis of continous variables :
# Lets check distribution of each numeric and categorical variables
num_var <- which( sapply(train_cab, is.numeric ))
#cnamestest <- cnamestest[-5]
num_var


# Histogram for continuous variables to check distribution of each variable:
num_var <- c("pickup_longitude" ,"pickup_latitude","dropoff_longitude","dropoff_latitude","distance")
num_var1 <- c("fare_amount","passenger_count")
col = rainbow(ncol(train_cab))
for (i in 1:length(num_var))
{
  print(ggplot(aes_string(x = num_var[i]), data=train_cab) +
          geom_histogram(fill=col[i], colour = "black",bins=100) + geom_density() +
#labs(title = "Fare Amount V/s Year", x = "Years", y = "FareAmount",subtitle = "Bi - Variate Analysis", caption = "(Based on Train data .)") +
theme_bw() + xlab(paste("Train Dataset - ",num_var[i]," Variable")) +
ylab(paste("Frequency Distribution of",num_var[i]," variable"))+
ggtitle(paste("Histogram type Distribution of",num_var[i],"variable of Train dataset")))
}

for(i in 1:length(num_var1))
{
  print(ggplot(aes_string(x = num_var1[i]), data=train_cab) +
          geom_histogram(fill=col[i], colour = "black",bins=30) + geom_density() +
#labs(title = "Fare Amount V/s Year", x = "Years", y = "FareAmount",subtitle = "Bi - Variate Analysis", caption = "(Based on Train data .)") +
theme_bw() + xlab(paste("Train Dataset - ",num_var1[i]," Variable")) +
ylab(paste("Frequency Distribution of",num_var1[i]," variable"))+
ggtitle(paste("Histogram type Distribution of",num_var1[i],"variable of Train dataset")))
}


#### ------BI-VARIATE ANALYSIS------ ###
# Bi-variate Analysis using Barplots:
#Reference Blog: https://ggplot2.tidyverse.org/reference/labs.html
#Bar plot for categorical and target variable:

# Converting all extracted new variables to Factors for better visualise in barplots.
Train_Cab1 = train_cab
Train_Cab1$month <- as.factor(Train_Cab1$month)
Train_Cab1$year <- as.factor(Train_Cab1$year)
Train_Cab1$date <-as.factor(Train_Cab1$date)
Train_Cab1$hour <- as.factor(Train_Cab1$hour)
Train_Cab1$minutes <- as.factor(Train_Cab1$minutes)
Train_Cab1$dayofweek <- as.factor (Train_Cab1$dayofweek)
Train_Cab1$seconds <- as.factor(Train_Cab1$seconds)
# Converting all extracted new variables to Factors because Characters are not supported in machine learning algorithms.
Test_Cab1=test
Test_Cab1$month <- as.factor(Test_Cab1$month)
Test_Cab1$year <- as.factor(Test_Cab1$year)
Test_Cab1$date <- as.factor(Test_Cab1$date)
Test_Cab1$hour <- as.factor(Test_Cab1$hour)
Test_Cab1$minutes <- as.factor(Test_Cab1$minutes)
Test_Cab1$dayofweek <- as.factor(Test_Cab1$dayofweek)
Test_Cab1$seconds <- as.factor(Test_Cab1$seconds)

# Visualization between fare_amount and years.
ggplot(data = Train_Cab1, aes(x = year, y = fare_amount, fill= year))+geom_bar(stat = "identity")+
labs(title = "Fare Amount V/s years", x = "Years", y = "Fare Amount",subtitle = "Bi - Variate Analysis",
caption = "(Observation : In year 2013 there were rides which got high fare_amount)") +
theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
theme(axis.text.x = element_text( color="Red", size=10, angle=0))+
theme(axis.text.y = element_text( color="Brown", size=10, angle=0))

# Visualization between fare_amount and months.
#col <- rainbow(ncol(Train_Cab))
ggplot(Train_Cab1, aes(x = month, y = fare_amount, fill= month))+geom_bar(stat = "identity")+
labs(title = " Fare Amount V/s. Month", x = "Months", y = "Fare Amount",subtitle = "Bi - Variate Analysis",
caption = "(Observation: Month May collects the highest fare_amount)")+
theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
theme(axis.text.x = element_text( color="Red", size=10, angle = 0))+
theme(axis.text.y = element_text( color="Brown", size=10, angle = 0))

# Visualization between fare_amount and weekday.
ggplot(data = Train_Cab1, aes(x = dayofweek,y = fare_amount, fill = dayofweek))+geom_bar(stat = "identity")+
labs(title = "Fare Amount Vs. Day of week", x = "Days of the week", y = "FareAmount",subtitle = "Bi - Variate Analysis",
caption = "(Observation : Thursday to Saturday rides has the highest fare_amount.)")+
theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
theme(axis.text.x = element_text( color="Red", size=10, angle=0)) +
theme(axis.text.y = element_text( color="Brown", size=10, angle=0))


# Visualization between fare_amount and Hours.
ggplot(data = Train_Cab1, aes(x = hour, y = fare_amount, fill = hour))+geom_bar(stat = "identity")+
labs(title = "Fare Amount Vs.Hour", x = "Hours", y = "Fare Amount",subtitle ="Bi - Variate Analysis",
caption = "(Observation : Rides taken during 6 pm to 11 pm gives highest fare_amount.)")+
theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
theme(axis.text.x = element_text( color="Red", size=10, angle=0))+
theme(axis.text.y = element_text( color="blue", size=10, angle=0))


# Visualization between fare_amount and date.
ggplot(data = Train_Cab1, aes(x = date, y = fare_amount, fill = date))+geom_bar(stat = "identity")+
labs(title = "Fare Amount Vs. Date", x = "Date", y = "Fare Amount",subtitle ="Bi - Variate Analysis",
caption = "(Observation : Rides taken during midweeks gives highest fare_amount.)")+
theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
theme(axis.text.x = element_text( color="Red", size=10, angle=0))+
theme(axis.text.y = element_text( color="blue", size=10, angle=0))


# Visualization between fare_amount and Minutes.
ggplot(data = Train_Cab1, aes(x = minutes, y = fare_amount, fill = minutes))+geom_bar(stat = "identity")+
labs(title = "Fare Amount Vs. Minutes", x = "Minutes", y = "Fare Amount",subtitle = "Bi - Variate Analysis",
caption = "(Observation: Rides pickups taken at 10-20-50 minutes of an hour fares were high)")+
theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
theme(axis.text.x = element_text( color="Red", size=6, angle=90))+
theme(axis.text.y = element_text( color="blue", size=10, angle=0))


# Visualization between fare_amount and Seconds.
ggplot(data = Train_Cab1, aes(x = seconds, y = fare_amount, fill = seconds))+geom_bar(stat = "identity")+
labs(title = "Fare Amount Vs. Seconds", x = "Seconds", y = "Fare Amount",subtitle = "Bi - Variate Analysis",
caption = "(Observation: Rides pickups fare amount does not impact with seconds)")+
theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
theme(axis.text.x = element_text( color="Red", size=6, angle=90))+
theme(axis.text.y = element_text( color="blue", size=10, angle=0))



###############Feature selection :######################
numeric_index <- sapply(train_cab,is.numeric)# Selecting only numeric
numeric_index
numeric_data <-train_cab[,numeric_index]
cnames <- colnames(numeric_data)
cnames

# Correlation Plot to select significant continous variables
numeric_index1 <- c("pickup_longitude" ,"pickup_latitude","dropoff_longitude","dropoff_latitude",
                    "passenger_count","distance","fare_amount")
res <- cor(train_cab[,numeric_index1])
round (res, 2)

corrgram(train_cab[,numeric_index1], order = F,diag.panel=panel.density,lower.panel=panel.cor,upper.panel=panel.pie,text.panel=panel.txt, main = "CORRELATION PLOT")

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- train_cab[, numeric_index1]
chart.Correlation(my_data, histogram=TRUE, pch=19)



##################### Anova Test :###################
#Anova Test is performed between cat_var (categorical independent variables) & fare_amount (continuous target variable)
str(train_cab)
cat_var<-c("month","year","date","dayofweek","hour","minutes","seconds","passenger_count")


# aov(Train_Cab$fare_amount~Train_Cab$year)
# Anova test for all categorical variables of training dataset:
for(i in cat_var){
  print(i)
Anova_test_result = summary(aov(formula = fare_amount~train_cab[,i],train_cab))
  print(Anova_test_result)
}
#names(train_cab)

#From the Anova result, we can observe Date, Dayofweek, minutes and seconds has p
#value > 0.05, so delete these variables, Hence not considering in model.
# let's delete date, dayofweek, minutes and seconds variables from train dataset
train_cab$dayofweek <- NULL
train_cab$date <- NULL
train_cab$minutes <- NULL
train_cab$seconds <- NULL

# let's delete date, dayofweek, minutes and seconds variables from Test dataset also
test$dayofweek <- NULL
test$date <- NULL
test$minutes <- NULL
test$seconds <- NULL
head(train_cab)
head(test)



####################Feature Scaling:########################
# numeric variable skewness can be checked using the function skewness from the e1071 library.
skewness(train_cab$pickup_longitude)
skewness(train_cab$dropoff_longitude)
skewness(train_cab$pickup_latitude)
skewness(train_cab$dropoff_latitude)
skewness(train_cab$distance) # for distance Variable
skewness(train_cab$fare_amount) # for fare_amount Variable
skewness(train_cab$passenger_count) # for passenger_count

#If the skewness of the predictor variable is 0, the data is perfectly symmetrical,
#If the skewness of the predictor variable is less than -1 or greater than +1, the data is highly skewed,
#If the skewness of the predictor variable is between -1 and -0.5 or between +1 and +0.5 then the data is moderately skewed,
#If the skewness of the predictor variable is -0.5 and +0.5, the data is approximately symmetric.
#Skewed data have a negative impact on linear regression. For example, if we take the scatter-
  #plot of distance variable we will see something very odd.

#From Skewness values we found that:
#distance(1.06), fare_amount(1.03) and passenger_count(2.078) are highly skewed.
#pickup_latitude(-0.43) , hour(-0.42), minutes(-0.011)and dropoff_latitude(-0.34) are moderately skewed.
#pickup_longitude(0.72) and dropoff_longitude(0.75) are moderately skewed.
library(ggplot2)
ggplot(train_cab, aes(x=distance, y=fare_amount)) + geom_point(alpha=0.5,color="Red")

#Scatterplot with scale_x_log10() and scale_y_log10()
ggplot(data = train_cab, aes(x = distance, y = fare_amount)) +geom_point(alpha=0.5,color="Green") +
scale_x_log10() + scale_y_log10()


##Let's create a linear regression model to predict fare_amount based on the distance using the raw values from the dataset.
lm.model = lm(fare_amount ~ distance, data = train_cab)
# Now, lets take a look into the summary:
summary(lm.model)

##Notice how the residual standard error is high: 3.574. Maybe a log-transformation in the values
#might help us to improve the model. For that, we will use the log1p function, which, by default,
#computes the natural logarithm of a given number or set of numbers.
lm_log.model = lm(log1p(fare_amount) ~ log1p(distance), data = train_cab)
# Now, lets take a look into the summary:
summary(lm_log.model)


##Lets take log transformation to remove skewness
# Lets define function for log transformation of variables
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}
xfinal = train_cab

yfinal = test

# Applying log function to distance variable of train dataset
train_cab$distance = signedlog10(train_cab$distance)
train_cab$fare_amount = signedlog10(train_cab$fare_amount)

# Applying log function to distance variable of test dataset
test$distance = signedlog10(test$distance)


# Checking distance variable of train dataset for distribution after applying log function
ggplot(train_cab, aes_string(x = train_cab$distance)) +
  geom_histogram(fill="skyblue", colour = "black", bins=30) + geom_density() + theme_bw() +
  labs(title = "Distribution of Distance after log transformation", x = "Distance", y = "Frequency",
  subtitle = "For Train Dataset", caption = "(Based on Train data )")+theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, face = "bold"))


# Checking fare_amount variable of train dataset for distribution after applying log function
ggplot(train_cab, aes_string(x = train_cab$fare_amount)) +
  geom_histogram(fill="skyblue", colour = "black", bins=30) + geom_density() + theme_bw() +
  labs(title = "Distribution of Fare Amount after log transformation", x = "Fare Amount", y = "Frequency",
  subtitle = "For Train Dataset", caption = "(Based on Train data )")+theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, face = "bold"))


# Checking distance variable of test dataset for distribution after applying log function
ggplot(test, aes_string(x = test$distance)) +
  geom_histogram(fill="Violet", colour = "black", bins=30) + geom_density() +
  labs(title = "Distribution of Distance after log transformation", x = "Distance", y = "Frequency",
  subtitle = "For Test Dataset", caption = "(Based on Test data)")+theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, face = "bold"))

xfinal1 = train_cab
yfinal1 = test
head(train_cab)
head(test)
str(train_cab)


##############Model development:###############
# Let's clean R Environment, as it uses RAM which is limited
#install.packages("DataCombine")
library(caret)
#rmExcept("Train_Cab")
#Split the data set into train and test set in 80:20 ratio.
set.seed(125)
train.index = createDataPartition(train_cab$fare_amount, p = .80, list = FALSE)
train = train_cab[train.index,]
test = train_cab[-train.index,]


##############Linear Regression model:##################
# fit linear regression model we will use the lm() function from stats package:
linear_Reg_model <- lm(fare_amount ~.,data = train)
summary(linear_Reg_model)

# Lets check the assumptins of ols regression
#Error should follow normal distribution and no hetroscadacity
# assumptions are checked usig residual plot and normal qq plot
# Change the panel layout to 2 x 2
par(mfrow = c(2, 2))
plot(linear_Reg_model)

# No multicolinearity between Independent variables
#vif check model
library(car)
vif(linear_Reg_model) # All vif values are less than 5 there is no multicolinearity among the IV

### All VIF values are less than 2 there is no multicolinearity among the variables
# No auto-correlation between errors
dwt(linear_Reg_model) # dwt < 2 so there is no autocorrelation in error
str(train)

# predicting for test data
predictions_LR <- predict(linear_Reg_model,test[,-10])
# For test data
print(postResample(pred = predictions_LR, obs =test$fare_amount))

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}
MAPE(test[,10],predictions_LR) # Fare amount variable at 10th position in train dataset.
#We can say LM model is around 84.37 % accurate


################Desicision Tree :#######################
library(rpart)# Popular decision tree???algorithm
#install.packages("rattle")
library(rattle)# Fancy tree plot
library(rpart.plot)# Enhanced tree plots
library(RColorBrewer)# Color selection for fancy tree plot
fit <- rpart(fare_amount ~.,data = train,method = 'anova')
fit
rpart.plot(fit, yesno = 2, digits=3, fallen.leaves=T, tweak = 1.5)

# predict for test cases
predictions_DTR <- predict(fit,test[,-10])
print(postResample(pred = predictions_DTR , obs =test$fare_amount))
MAPE(test[,10],predictions_DTR)
#We can say our decision tree model around 87.80% accurate

################### Random Forest :#####################
#It will take 3 min to execute the code
#install.packages("randomForest")
library(randomForest)
RF_model = randomForest(fare_amount ~.,data = train, importance = TRUE, ntree = 200)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-10])
print(postResample(pred = RF_Predictions , obs =test$fare_amount))
MAPE(test[,10],RF_Predictions)
#Random forest model score is 90.08% accurate

## Model results:
LinearRegression = c(RMSE =0.1682 ,Rsquared =0.4337 , MAE =0.1237 , MAPE =0.1563 , Accuracy = (1-0.1563)*100)
DecisionTree = c(RMSE =0.1397 ,Rsqared =0.6095 , MAE =0.103 , MAPE =0.1220 , Accuracy = (1-0.1220)*100)
RandomForest = c(RMSE =0.1181 ,Rsquared =0.7211 , MAE =0.080 , MAPE =0.0992 , Accuracy = (1-0.0992)*100)
Final_Results_in_R <- cbind(LinearRegression,DecisionTree,RandomForest)
t(Final_Results_in_R)

#From Results we can say Random forest model is performing well and having optimum
#values compared to rest other models on train dataset. So, will freeze RF model for predictions.

##########Model evaluation #############
str(train_cab)

# Model evaluation using this test data : start with complete train data to train our model.
RFModel = randomForest(fare_amount~., train_cab, ntree = 200, method = "anova")
# Predicting model on test data
RFtest = predict(RFModel, test)
# Adding our obtained predictions as Predicted Fare Amount variable to test_cab dataset
test$Predicted_fare_amount = RFtest
# Here's a glance of predicted values
head(test)
summary(test$Predicted_fare_amount)
summary(train_cab$fare_amount) # predicted fare amount are not actual values hence making it to actuals

# Applying 10^ function to distance and Predicted_fare_amount variable of train dataset to get actual values.
train_cab$distance = round(10^(train_cab$distance),6)
train_cab$fare_amount = round(10^(train_cab$fare_amount),2)

# Applying 10^ function to distance and Predicted_fare_amount variable of test dataset to get actual values.
test$distance = round(10^(test$distance),6)
test$Predicted_fare_amount = round(10^(test$Predicted_fare_amount),2)

#train_cab <-train_cab[,c(2:10,1)]
head(train_cab)
head(test)
summary(test$Predicted_fare_amount)
summary(train_cab$fare_amount)
str(train_cab)

# Finally, we designed a model, which predicts the cab fare.
# Exporting the output to hard disk for further use
write.csv(test, "C:/Users/Rahul/Documents/output_carfare_prediction.csv",row.names = FALSE)

