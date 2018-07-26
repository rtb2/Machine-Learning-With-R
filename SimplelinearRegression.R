#Simple Linear Regresseion

Salary = read.csv(file.choose())

#Splitting the data into training and test sets
library(caTools)
set.seed(123)
split = sample.split(Salary$Salary,SplitRatio = 2/3)
training_set = subset(Salary, split==T)
test_set = subset(Salary, split==F)

#Fitting simple linear regression to the Training Set
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

#predecting the test set results
Y_pred = predict(regressor,newdata = test_set)
Y_pred
?predict

#Visualising Traing set
library(ggplot2)

ggplot()+
  geom_point(aes(x=training_set$YearsExperience, y=training_set$Salary),
             color='red')+
  geom_line(aes(x=training_set$YearsExperience, 
                y =predict(regressor,newdata = training_set)),
            color='blue')+
  ggtitle('Salary Vs Experience (Training Set)')+
  xlab("Years of Experience")+
  ylab("Salary")+
  geom_smooth()

#Visualising Test set
ggplot()+
  geom_point(aes(x=test_set$YearsExperience, y=test_set$Salary),
             color='red')+
  geom_line(aes(x=training_set$YearsExperience, 
                y =predict(regressor,newdata = training_set)),
            color='blue')+
  ggtitle('Salary Vs Experience (Test Set)')+
  xlab("Years of Experience")+
  ylab("Salary")
  
