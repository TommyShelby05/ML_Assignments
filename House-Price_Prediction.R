## Importing the Data
df <- read.csv(file.choose(),header = TRUE)
View(df)

## Renaming the columnns names
colnames(df) <- c('no','transaction_date','house_age','MRT_station_distance','no_of_convenience_stores','latitude','longitude','Y')
## Str of Data
str(df)
## To check null
sum(is.na(df))
## Descriptive statistics of Data and check whether there is a null or not
summary(df)
library(psych)
describe(df)
##
library(ggplot2)
pairs(df[,c(2,3,4,8)], col= "red", pch=18, main= "Relationship between first four explanatory variables")
hist(df$house_age)
hist(df$MRT_station_distance)
MRT = log10(df$MRT_station_distance)
df$MRT_station_distance = MRT
hist(df$MRT_station_distance)

install.packages("corrplot")
library(corrplot)
correlations <- cor(df[,2:8])
corrplot(correlations,method = "number",bg="black") 
corr <- cor(df)
install.packages("caTools")
library(caTools)
new <- df[ , c("house_age", "MRT_station_distance","no_of_convenience_stores","Y")]
sample <- sample.split(new$Y,SplitRatio = 2/3)
train  <- subset(new, sample == TRUE)
test   <- subset(new, sample == FALSE)
MAIN <- glm(Y~house_age +MRT_station_distance +no_of_convenience_stores ,data = train)

train[1:10,]
install.packages("caret")
library(caret)
process <- preProcess(as.data.frame(new), method=c("range"))
norm_scale <- predict(process, as.data.frame(new))

summary(MAIN)

