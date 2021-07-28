# Title: Capstone Project: Video Games Sales with Ratings
# Author: Rahasya Chandan

#  Read dataset
vgames <- read.csv("https://raw.githubusercontent.com/rahasyac/Data_Science_Capstone_Project/main/Data%20Science%20Capstoe%20Project/Data/Video_Games_Sales_as_at_22_Dec_2016.csv") # read dataset

library(tidyr)
library(dplyr)

dropcolumns <- c("Name","Year_of_Release","Developer","Publisher")# drop columns
vgamesdf<- vgames[,!(names(vgames) %in% dropcolumns)]
vgamesdf <- vgamesdf %>% mutate_all(na_if,"")

head(vgamesdf)# print data

# drop NA values/rows
vgamesdf <- vgamesdf %>% drop_na()

# data summary
summary(vgamesdf)

library(ggplot2)
plots <-ggplot(data = vgamesdf,aes(x = Critic_Score,y=User_Score,color = Platform))
plots + geom_point() + geom_smooth(fill = NA) + ggtitle("Critic vs User Score")+ 
  xlab("Critic Score")+
  ylab("User Score")+
  theme(axis.title.x = element_text(color="red",size = 35),
        axis.title.y = element_text(color="purple",size = 35),
        legend.title = element_text(color="purple",size = 15))

vgamesdf %>% count(Rating)# count observation for each Rating category
# the results shown that 
#Rating    n
#1     AO    1
#2      E 2118
#3   E10+  946
#4    K-A    1
#5      M 1459
#6     RP    2
#7      T 2420
# therefore, AO, K-A and RP instances are removed because the machine learning model will be bias as there are not enough observations
vgamesdf <- filter(vgamesdf, Rating != "AO")
vgamesdf <- filter(vgamesdf, Rating != "K-A")
vgamesdf <- filter(vgamesdf, Rating != "RP")

# convert variables to factor and numeric
vgamesdf$Platform <- as.factor(vgamesdf$Platform) 
vgamesdf$Genre <- as.factor(vgamesdf$Genre)
vgamesdf$Rating <- as.factor(vgamesdf$Rating)
vgamesdf$User_Score <- as.numeric(vgamesdf$User_Score)
vgamesdf$Critic_Score <- as.numeric(vgamesdf$Critic_Score)
vgamesdf$Critic_Count <- as.numeric(vgamesdf$Critic_Count)
vgamesdf$User_Count <- as.numeric(vgamesdf$User_Count)

str(vgamesdf)
# convert string variables to numeric representation
df <- vgamesdf %>% mutate_if(is.factor, as.numeric)
df <- df[sample(nrow(df)),]# randomly shuffle dataset

# split dataset
#install.packages('caTools')
library(caTools)

set.seed(12345)
split = sample.split(df$Rating, SplitRatio = 0.70)

training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# data scaling
training_set[-12] = scale(training_set[-12])
test_set[-12] = scale(test_set[-12])

# Multiple regression model

ggplot(training_set, aes(Rating)) + geom_density(fill="blue")

# Take all the inputs to make a multiple regression model 
model1 = lm(Rating~., data=training_set)

#data summary
summary(model1)

# plot the model
par(mfrow=c(2,2))
plot(model1)

# prediction based on model 1
pred1 <- predict(model1, newdata = test_set)

# calculate RSME and R^^2 
rmse <- sqrt(sum((exp(pred1) - test_set$Rating)^2)/length(test_set$Rating))
result1 <- c(RMSE = rmse, R2=summary(model1)$r.squared)
result1

# plot the model
par(mfrow=c(1,1))
plot(test_set$Rating, exp(pred1))

#install.packages('e1071')
library(e1071)

# support vector machine model
classifier = svm(formula = Rating ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

y_pred = predict(classifier, newdata = test_set[-12])


# confusion matrix
cm = table(test_set[, 12], y_pred)
cm

library(caret) 
result2 <- confusionMatrix(cm)

# result for Multiple regression model
result1

# result for support vector machine
result2

