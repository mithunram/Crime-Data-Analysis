#loading Required Libraries
library(data.table)
library(arules)
library(xgboost)
library(caret)
library(lubridate)
#install.packages("ggplot2")
library(ggplot2)
library(MLmetrics)
#require(foreign)
library(nnet)
#Reading Data
Boston_Crime_Data <- read.csv("~/Documents/Infor - Boston Crime Data.csv", na.strings=c("NA","NaN", "NULL"," ", "NAN"))


names(Boston_Crime_Data)

#######

#Summary of Data & Missing Values
x=Boston_Crime_Data
summary(x)

#Total Missing Values
missing_df<-sapply(x, function(x) sum(is.na(x)))
missing_df<-as.data.frame(missing_df)
missing_df
#colnames(missing_df)<- "No_of_Missing_Values"
#missing <- missing_df[order(-No_of_Missing_Values)]

# Variable Selection
Boston_Crime_Data$COMPNOS =NULL
Boston_Crime_Data$NatureCode =NULL
Boston_Crime_Data$X =NULL
Boston_Crime_Data$Y =NULL
Boston_Crime_Data$Location = as.character(Boston_Crime_Data$Location)
tmp = strsplit(gsub(".*\\((.*)\\).*", "\\1", Boston_Crime_Data$Location), ',')
tmp = matrix(unlist(tmp), ncol = 2, byrow=T)
Boston_Crime_Data$lat = as.numeric(tmp[, 1])
Boston_Crime_Data$lng = as.numeric(tmp[, 2])

#Library to parse date 

#Boston_Crime_Data$DateFormatted = parse_date_time(Boston_Crime_Data$FROMDATE, orders="mdy hm")

Boston_Crime_Data$DateFormatted = mdy_hm(Boston_Crime_Data$FROMDATE, tz='EST')

Boston_Crime_Data$FROMDATE= NULL
Boston_Crime_Data$REPORTINGAREA = NULL
Boston_Crime_Data$DOMESTIC = NULL
Boston_Crime_Data$Location = NULL

# Assigning categories for Crime Code
setDT(Boston_Crime_Data)[, id := .GRP, by =MAIN_CRIMECODE ]
write.csv(Boston_Crime_Data, file = "Rawdata.csv")

library(sqldf)
sqldf1 <- sqldf("SELECT Boston_Crime_Data.id, Boston_Crime_Data.MAIN_CRIMECODE
                FROM Boston_Crime_Data GROUP BY Boston_Crime_Data.id")

#Removing INCIDENT_TYPE_DESCRIPTION & MAIN_CRIMECODE as they co-related to response 
Boston_Crime_Data$INCIDENT_TYPE_DESCRIPTION=NULL
Boston_Crime_Data$MAIN_CRIMECODE=NULL
Boston_Crime_Data$Year = as.factor(Boston_Crime_Data$Year)
Boston_Crime_Data$Month = as.factor(Boston_Crime_Data$Month)
Boston_Crime_Data$hr = strftime(Boston_Crime_Data$DateFormatted, format='%H')
Boston_Crime_Data$AtNight=(strftime(Boston_Crime_Data$DateFormatted, format='%H:%M:%S') > "19:00:00")|
  (strftime(Boston_Crime_Data$DateFormatted, format='%H:%M:%S') < "07:00:00")

cutpoints = c(-1, 6, 12, 18, 24)
Boston_Crime_Data$timeDec = cut(as.numeric(Boston_Crime_Data$hr), cutpoints)
# UCRPART -- We had an upper case part 3 and lower case part 3 so to merge them
Boston_Crime_Data$UCRPART = as.factor(toupper(Boston_Crime_Data$UCRPART))

summary(Boston_Crime_Data)


#Checking for Type of class of levels
lapply(Boston_Crime_Data,class)
lapply(Boston_Crime_Data,nlevels)

# Changing class Types
Boston_Crime_Data$Month = factor(Boston_Crime_Data$Month)
Boston_Crime_Data$Year= factor(Boston_Crime_Data$Year)
Boston_Crime_Data$hr = factor(Boston_Crime_Data$hr)




#### Intital EDA #####

street = data.frame(table(Boston_Crime_Data$STREETNAME))
colnames(street)=c('Name', 'Freq')
street$Name = as.character(street$Name)

# remove street names that are blank
street = street[-which(nchar(street$Name)==0), ]

library(wordcloud)
wordcloud(street$Name, street$Freq, min.freq = 1000, scale=c(2,.5),random.order=F, 
          random.color=T, colors=c('black', 'red', 'steelblue'))

# Maximum Frequency is Washington ST

# Histogram of day week with hour slice
ggplot(data=Boston_Crime_Data, aes(x=Boston_Crime_Data$DAY_WEEK, fill=Boston_Crime_Data$hr))+
  geom_bar(stat="count")

#Histogram of Hour of day with Week slice
ggplot(data=Boston_Crime_Data, aes(x=Boston_Crime_Data$hr, fill=Boston_Crime_Data$DAY_WEEK))+
  geom_bar(stat="count")

ggplot(data=Boston_Crime_Data, aes(x=Boston_Crime_Data$WEAPONTYPE, fill=Boston_Crime_Data$WEAPONTYPE))+
  geom_bar(stat="count")
#Unarmed Crimes are higher 


#qplot(Boston_Crime_Data$hr,
#      geom="bin",
#     binwidth = 0.5,
#    main = "Histogram Of Waepons used")

ggplot(data=Boston_Crime_Data, aes(x=Boston_Crime_Data$WEAPONTYPE, y=Boston_Crime_Data$hr, fill=Boston_Crime_Data$hr)) +
  geom_bar(stat="identity")

library(lattice)
Boston_Crime_Data$hr = as.factor(Boston_Crime_Data$hr)
histogram(~hr|WEAPONTYPE, data=Boston_Crime_Data)




#Boston_Crime_Data$id=as.integer(Boston_Crime_Data$id)

summary(Boston_Crime_Data)

head(Boston_Crime_Data)



Boston_Crime_Data$id=as.factor(Boston_Crime_Data$id)

#attach(Boston_Crime_Data)

#Model Building:
 

#Feature Selection

#Boston_Crime_Data$Ddays=NULL
#Boston_Crime_Data$lat=NULL
#Boston_Crime_Data$lng=NULL
Boston_Crime_Data$DateFormatted=NULL
Boston_Crime_Data$XSTREETNAME=NULL
Boston_Crime_Data$UCRPART=NULL
#Boston_Crime_Data$lat=NULL


#Sampling of Data - Into Train & test


#attach(Boston_Crime_Data)

split=0.70
trainIndex <- createDataPartition(Boston_Crime_Data$id, p=split, list=FALSE)
data_train2 <- Boston_Crime_Data[ trainIndex,]
data_test2 <- Boston_Crime_Data[-trainIndex,]


data_train3 <- subset(data_train2, data_train2$id %in% c("4",	"5",	"1",	"8",	"18",	"15",	"3",	"6",	"12",	"2",	"16",	"20",	"19",	"35",	"11",	"9"),
                       select=REPTDISTRICT:timeDec)
#as.factor(data_train3$id)
data_test3 <- subset(data_test2, id %in% c("4",	"5",	"1",	"8",	"18",	"15",	"3",	"6",	"12",	"2",	"16",	"20",	"19",	"35",	"11",	"9"),
                      select=REPTDISTRICT:timeDec)


#levels(data_train2$MAIN_CRIMECODE)

#summary(data_train2)

data_train2 <- na.omit(data_train2)
data_test2 <-na.omit(data_test2)
data_train2 <- as.data.frame(data_train2)
names(data_train2)

xgb <- xgboost(data = data.matrix(data_train2[,-12]), 
               label = data_train2$id, 
               eta = 0.4,
               max_depth = 7, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               eval_metric = "mlogloss",
               objective = "multi:softmax",
               num_class = 78,
               nthread = 3
)


y_pred <- predict(xgb, data.matrix(data_test2))

names <- dimnames(data_train2)[[2]]

names<- names[-12]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)

# Importance Graph 
xgb.plot.importance(importance_matrix[1:10,])

write.csv(y_pred, file = "Result.csv")
write.csv(data_test2$id, file= "Test_id.csv" )

xx=MultiLogLoss(y_pred,data_test2$id)
#MultiLogLoss(y_true = data_test2$id, y_pred = y_pred)

head(data_test2)
head(data_test2$id)
# Calculating Mlogloss on test data 



y_pred=as.numeric(y_pred)
data_test2$id = as.numeric(data_test2$id)
mlogloss = function(actual, pred_m, eps = 1e-15){
  
  if(max(actual) >= ncol(pred_m) || min(actual) <2){
    stop(cat('True labels should range from 0 to', ncol(pred_m) - 1, '\n'))
  }
  
  pred_m[pred_m > 1 - eps] = 1 - eps
  pred_m[pred_m < eps] = eps
  pred_m = t(apply(pred_m, 1, function(r)r/sum(r)))
  actual_m = matrix(0, nrow = nrow(pred_m), ncol = ncol(pred_m))
  actual_m[matrix(c(1:nrow(pred_m), actual + 1), ncol = 2)] = 1
  -sum(actual_m * log(pred_m))/nrow(pred_m)
}

NormalizedGini(y_pred,data_test2$id)