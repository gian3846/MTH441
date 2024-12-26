###############################
#Q1

#Linear Regression
#y = beta_0 + beta_1 * x + e
library(readxl)
n = 20
data <- read_xlsx( "Electricity_Data.xlsx")
x <- data$X
y <- data$Y

#estimate model parameters

S_xy = sum(x*x) - (sum(x))^2/n
S_xx = sum(x*y) - (sum(x)*sum(y))/n


beta_1 = S_xy/S_xx
beta_0 = mean(y) - beta_1 * mean(x)




#Fitted Values

y_fit = beta_0 + beta_1 * x
y_fit

#residual

e= y - y_fit
e

#Q3
#sigma_sq estimation

ss_t = sum(y*y) - sum(y)^2/n
ss_r = ss_t - (beta_1*v2)
ss_r = sum(e*e)

sigma_sq = ss_r/(n-2)
sigma_sq


#t_j

c = (x - mean(x))/v1

var1 = sum(c * var(y))
s = var(y)
var0 = s*(1/n + mean(x)^2/v1)
t_0 = beta_0/var0 
t_1 = beta_1/var1

#############################################
#Q2

library(readxl)

data <- read_xlsx( "TimeDeliveryData.xlsx")

y <- as.matrix(data$y)
x1 <- data$x1
x2 <- data$x2
one <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

X <- matrix(data =c(one,x1,x2),ncol = 3,nrow = 25)
X
t

library(ggplot2)

pairs(data[,c(2,3:4)])

beta_e <- qr.solve(t(X)%*%X)%*%t(X)%*%y
beta_e

y_fit <- beta_e[1,1] + beta_e[2,1]*x1 + beta_e[3,1]*x2

#####################################
#Q4

set.seed(123)

z1 <- rnorm(5000,0,1)
z2 <- rnorm(5000,0,1)
z3 <- rnorm(5000,0,1)

w <- z1^2 + z2^2 + z3^2
w

hist(w)
mean(w)
var(w)

# For a chi-square RV
# k degrees of freedom,
# 
# Mean = k
# Variance = 2k
# 
# Theoretical Mean = 3
# Theoretical Var = 6
###################################

#Q5
library(MASS)

X = matrix(rnorm(8 * 5), nrow = 8, ncol = 5)

Px = X%*%qr.solve(t(X)%*%X)%*%t(X)

all.equal(Px%*%Px ,Px) #yes idempotent


I <- diag(8)
mu <- matrix(data = rep(0,8),nrow = 8,ncol = 1)
dim(mu)
Y <- mvrnorm(1,mu,I)

dim(Px)

u <- t(Y)%*%Px%*%Y

generate_sample <- function() {
  Y <- mvrnorm(n = 1, mu = mu, Sigma = I)
  u <- t(Y) %*% Px %*% Y
  return(u)
}

sample <- replicate(5000,generate_sample())
sample

hist(sample)
mean(sample)
var(sample)

#theoretical mean = 5
#var = 10

##############################################

#Q6
library(Matrix)

I <- diag(8)
mu <- matrix(data = rep(0,8),nrow = 8,ncol = 1)
dim(mu)
Y <- mvrnorm(1,mu,I)

dim(Px)

Px1 <- Px
Px2 <- I - Px1

df1 <- as.numeric(rankMatrix(Px2))
df2 <- as.numeric(rankMatrix(Px1))


generate_sample <- function() {
  Y <- mvrnorm(n = 1, mu = mu, Sigma = I)
  numerator <- t(Y) %*% Px2 %*% Y / df1
  denominator <- t(Y) %*% Px1 %*% Y / df2
  f <- numerator/denominator
  return(f)
}

res <- replicate(5000,generate_sample())
hist(res,main = "Histogram of Samples",xlim = c(0,20),ylim = c(0,3500),breaks = 40)

mean(res)
var(res)

#theoretical mean = df2/(df2 - 2)  (for df2>2)
#mean = 5/5-2 = 1.67
#variance = (2.df^2.(df1 + df2 - 2))/df1.(df2 - 2)^2.(df2 - 4)  (for df2>4)
# = 300/27 = 11.11









