#Question 1
#============
library(readxl)
library(tidyr)
data = read_excel('Electricity_Data.xlsx')
colnames(data) = c('X','Y')
model = lm(Y~X, data)

library(ggplot2)

g1 = ggplot()+
  geom_point(aes(model$fitted.values, model %>% rstudent() ))+
  labs(title = 'Residual Plot',
       x = 'Yhat',
       y = 'e')

#find optimal lambda for Box-Cox transformation 
library(MASS)
box_cox <- boxcox(Y ~ X, data = data)
(lambda <- box_cox$x[which.max(box_cox$y)])

#transformed model
Y1 = (data$Y^lambda-1)/(lambda * prod(data$Y)^((lambda-1)/n))
model_bc = lm(Y1~data$X)

g2 = ggplot()+
  geom_point(aes(model_bc$fitted.values, model_bc %>% rstudent() ))+
  labs(title = 'Residual Plot after Box-Cox transformation',
       x = 'Yhat',
       y = 'e')

library(patchwork)
g1+g2
#=============================================

#Question 2
library(readxl)
data = read_excel('Wind_Mill_Data.xlsx')

model = lm(Y~X, data)

library(ggplot2)

g3 = ggplot()+
  geom_point(aes(model$fitted.values, model %>% rstudent() ))+
  labs(title = 'Residual Plot',
       x = 'Yhat',
       y = 'e')

library(car)
#optimal value of alpha for box-tidwell transformation
bt = boxTidwell(Y~X, data = data)
alpha = bt$result[1]

X1 = data$X^alpha

model_bt = lm(data$Y~X1)

g4 = ggplot()+
  geom_point(aes(model_bt$fitted.values, model_bt %>% rstudent() ))+
  labs(title = 'Residual Plot after Box-Tidwell transformation',
       x = 'Yhat',
       y = 'e')

library(patchwork)
g3+g4
