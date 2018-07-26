#importing data
Salary = read.csv(choose.files())

#Removing Positin as it is already taken as levels
Salary$Position = NULL

#fitting Salary to liner regression
linReg = lm(formula=Salary~Level, data = Salary)
predict(linReg,newdata = 6.5)
#fitting Salary to Ploynomial regression
Salary$Level2 = Salary$Level^2
Salary$Level3 = Salary$Level^3
Salary$Level4 = Salary$Level^4
polyReg = lm(formula=Salary~., 
             data = Salary)


#Visualising Linear Regression results
library(ggplot2)
ggplot()+
  geom_point(aes(x=Salary$Level, y=Salary$Salary), color="red")+
  geom_line(aes(x=Salary$Level, y=predict(linReg, newdata = Salary)), color = 'blue')

#Visualising Polynomial Regression results
ggplot()+
  geom_point(aes(x=Salary$Level, y=Salary$Salary), color="red")+
  geom_line(aes(x=Salary$Level, y=predict(polyReg, newdata = Salary)), color = 'blue')

#Predicting the value with linear regreesion model
y_pred = predict(linReg, newdata = data.frame(Level = 6.5))

#Predicting the value with polynomial regreesion model
y_pred_poly = predict(polyReg, newdata = data.frame(Level = 6.5,
                                                    Level2 = 6.5^2,
                                                    Level3 = 6.5^3,
                                                    Level4 = 6.5^4))


