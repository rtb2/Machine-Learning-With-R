# Import Dataset
Startup = read.csv(file.choose())

#Encoding Categorical variables
# "California" =1 "Florida"=2   "New York"=3 
Startup$State = factor(Startup$State,
                       levels = levels(Startup$State),
                       labels =  c(1,2,3))
#Splitting the dataset into training and test sets
library(caTools)
set.seed(123)
split = sample.split(Startup$Profit, SplitRatio = 0.8)
training_set = subset(Startup, split==T)
test_set = subset(Startup, split==F)

#Fitting multiple linear regression to training set
regressor = lm(formula = Profit~., data=training_set)
summary(regressor)

#predicting test results
Y_pred = predict(regressor,newdata = test_set)

#Building optimal model using Backword Elemination
summary(regressor)
regressor = lm(formula = Profit~R.D.Spend+Administration+Marketing.Spend+State, 
               data=Startup)
summary(regressor)

#State3 has highest P value and higher than SL of 0.05, so remove it
regressor = lm(formula = Profit~R.D.Spend+Administration+Marketing.Spend, 
               data=Startup)
summary(regressor)

#Administration has highest P value and higher than SL of 0.05, so remove it
regressor = lm(formula = Profit~R.D.Spend+Marketing.Spend, 
               data=Startup)
summary(regressor)

#Marketing Spend has highest P value and higher than SL of 0.05, so remove it
regressor = lm(formula = Profit~R.D.Spend, 
               data=Startup)
summary(regressor)

regressor = lm(formula = Profit~R.D.Spend+Marketing.Spend, 
               data=training_set)
Y_pred = predict(regressor,test_set)
Y_pred
