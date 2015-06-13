# Loading data
## *.csv
train <- read.csv("input/train.csv", header = TRUE/FALSE, skip = 4, nrow = 190, stringsAsFactors=FALSE)    # skip x rows, read nrows
write.csv(submission, file = "filename", row.names=FALSE)

## concatenate file path
data_dir <- "input/"
train <- read_csv(file.path(data_dir, "train.csv"))

## downloading file
download.file("url","filename to save as.csv")

## Excel
library(xlsx) 
data <- read.xlsx("data.xlsx", SheetIndex = 1, rowIndex = c(<numeric>), colIndex = c(<numeric>))

## JPEG
library(jpeg)
data <- readJPEG("Fjeff.jpg", native = TRUE)

## .txt
df <- read.table("data.txt")

## JSON
library(jsonlite)
test <- fromJSON(".rpushbullet.json")

apikey <- "N8o2VzzUEH991SpFv4YgqboXFCW438Kt"                ## replace with the string of your API key
test <- pbGetDevices()
test$devices[1,c("iden", "nickname")]

txt <- list(key = apikey, devices = c("ujBYgEiABvosjz7O3P0Jl6","ujBYgEiABvosjAiVsKnSTs"), names = c("Chrome","Chrome"))
exportJson <- toJSON(txt)
write(exportJson, ".rpushbullet.json")

## adding in external function library
source("filename.R")

# Data manipulation
## Defining a new column in a data frame with a given name
submission <- data.frame(datetime=test$datetime, count=NA)    # <column name> = values
submission[, "count"] <- predict(rf, extractFeatures(testSubset))

## Splitting a string
unlist(strsplit("a.b.c", "[.]"))

## Replacing parts of a string
gsub(",","","t,est,ing")
gsub("[()]","","testing()")



## Test whether a pattern is present
grep(",","t,est,ing") # returns index
grepl(",","t,est,ing") # returns Boolean
namel <- grepl("^United", countryName)   # Starts with ...

## finding indexes
which(vector of Booleans)
which.max(x)
which.min(x)

## range
range(x, na.rm=T)     # returns minimum and maximum

## merging data frames
data_merged <- merge(df1, df2, by.x = "", by.y = "")

## Arranging a data frame according to a vector
arrange(data, Date,Trap,Species,desc(NumMosquitos))  # arranges by Date first (in ascending order). then breaks ties using subsequet inputs

## split / unsplit
l <- split(airquality,list(airquality$Month,airquality$Day),drop=T)     # produces a list where data is split up according to the unique values of data$p
l2 <- lapply(l,max,na.rm=T)
#l3 <- unsplit(l2,list(airquality$Month,airquality$Day))     # undoes the split (includes any information)
l3 <- cbind(read.table(text = names(l2), sep = ".", colClasses = "character"), value = unlist(l2))

## Column names
names(data) <- c("name column1", "name column 2",...)
colnames(l3)[3] <- "test"    # change a particular column name

## Delete row/column
data[i,] <- NULL  # delete row
data[,i] <- NULL  # delete column

## Replacing a factor with an integer
x <- as.factor(c("f1", "f2","f2","f1"))
x <- match(x, levels(x), nomatch = "f3")

## dplyr --> replaces cbind ... MUCH BETTER!
single_date_grouped_by_location <- train %>% 
  filter(Date == "2007-08-01") %>%          # select only those rows that satisfy the logical
  group_by(Longitude, Latitude) %>%         # group by combinations of longitude and latitude (can be multiple factors)
  summarize(NumMosquitos = sum(NumMosquitos))     # perform function according to groups

## subset
data_subset <- subset(data, subset = (logical for rows to keep), select = c(list of column names to keep))

## Table
crimes_by_day <- table(data$Category,data$DayOfWeek)   # counts for all combinations
crimes_by_day <- melt(crimes_by_day)     # creates a row for each unique combination
dcast(crimes_by_day,Var1~Var2)

## rbind
rbind(x1,x2)
cbind(x1,x2)

set.seed(1)

# Preparing features (Ben Hammer)
extractFeatures <- function(data) {
  features <- c("season",
                "holiday",
                "workingday",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed",
                "hour")
  data$hour <- hour(ymd_hms(data$datetime))
  return(data[,features])
}

trainFea <- extractFeatures(train)

# Statistical modeling
## Random Forest
rf <- randomForest(trainFeatures, Labels, ntree=100,importance=TRUE)   # make sure there are no NA's and all values are numeric!
predict(rf, testFeatures)
imp <- importance(rf, type=1)   # importance(x, type=NULL, class=NULL, scale=TRUE)
                                # type: 1 for mean decrease in accuray, 2 for mean decrease in node impurity [NULL]
                                # class: classification problem [NULL]
                                # scale: should the measures be divided by their standard errors? Y/N [TRUE]


## K Nearest Neighbors
library(class)
cl=factor(train2$Survived)
Survived_pred <- knn(trainFea, testFea, cl, k = 11)

## Logistic regression
library(aod)   # necessary?
survived.glm = glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                   data = train2,
                   family = binomial)

Survived_pred_train <- predict(survived.glm, trainFea, type="response")

## Naive Bayes
library(e1071)
classifier<-naiveBayes(trainFea, train2$Survived, laplace = 2)
Survived_pred <- predict(classifier,testFea, type = "raw")
Survived_pred <- as.numeric(Survived_pred[,2]>0.5)

## Artificial Neural Networks
library(neuralnet)
model <- neuralnet(trainSurvived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, train2, hidden = 10)
Survived_pred <- compute(model, testFea)
Survived_pred <- Survived_pred$net.result

# for loop
for (i in 1:10) {
  ...
}

# Plotting
p <- ggplot(data, aes(x=, y=)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("") + 
  ggtitle("Title\n") +
  theme(plot.title=element_text(size=18))

ggsave("filename.png", p)

# Utility
cat(...,...)  # concatenate strings/values and print

work_dir <- getwd()

library(RPushbullet)
pbPost("note", "It's working", "Yay!", recipients = 2)  # post a note to PushBullet, title, body, device indicator (2 = phone))


# Parallel computing
library(foreach)
im.train <- foreach(im = im.train, .combine=rbind) %dopar% {     # .combine --> how should the results be combined?
  as.integer(unlist(strsplit(im, " ")))                          # can also use %do%, but executes sequentially instead of parallel
}

# Setting up cross validation
idxs     <- sample(nrow(data), nrow(data)*0.8)   # 80% in training set
train  <- data[idxs, ]
test   <- data[-idxs, ]

# Dates (with lubridate)
train$Year <- year(ymd(train$Date))
train$Month <- month(ymd(train$Date))
train$Day <- day(ymd(train$Date))

ymd_hms()
interval(t_start,t_end)
t %within% interval
