###############################
# Q1: Simple Linear Regression
library(readxl)
dataset <- read_excel("Electricity_Data.xlsx")
x <- dataset$X
y <- dataset$Y
n <- length(x)
model <- lm(Y ~ X, data = dataset)
summary(model)
S_xy <- sum(x * y) - (sum(x) * sum(y)) / n
S_xx <- sum(x^2) - (sum(x)^2) / n
beta_1 <- S_xy / S_xx
beta_0 <- mean(y) - beta_1 * mean(x)
y_fit <- beta_0 + beta_1 * x
residuals <- y - y_fit
plot(x, y)
abline(a = beta_0, b = beta_1, col = 'blue')

#######################################
# Q2: Multiple Linear Regression

library(ggplot2)
data <- read_excel("TimeDeliveryData.xlsx")
y <- data$Y
x1 <- data$X1
x2 <- data$X2
model <- lm(Y~X1+X2, data=data)
summary(model)
# Calculate fitted values and residuals
fitted_values <- predict(model)
residuals <- y - fitted_values
# Plot relationships
pairs(data[, c("X1", "X2", "Y")])
# Print additional details
cat("Fitted values:\n", fitted_values, "\n")
cat("Residuals:\n", residuals, "\n")

######################################## 
#Q3 Residuals and model parameters
residuals<-model$residuals
n<-length(residuals)
p<-length(model$coefficients)

# Unbiased estimator of sigma^2
sigma2<-(sum(residuals^2))/(n-p)

# Create design matrix and ensure it's numeric
X<-as.matrix(cbind(rep(1,n),data[,c("X1","X2")]))
C<-solve(t(X)%*%X)

# t-statistics
betas<-model$coefficients
t_stats<-betas/sqrt(sigma2*diag(C))
t_stats
#######################################
# Q4: Chi-Square Distribution

n <- 5000  # Number of samples
Z1 <- rnorm(n)  # Standard normal samples
Z2 <- rnorm(n)
Z3 <- rnorm(n)
W <- Z1^2 + Z2^2 + Z3^2  # Chi-square variable
hist(W, breaks=50, main="Histogram of W", xlab="W", col="blue")  # Plot histogram
mean_W <- mean(W)  # Sample mean
var_W <- var(W)  # Sample variance
theoretical_mean_W <- 3  # Theoretical mean
theoretical_var_W <- 6  # Theoretical variance
mean_diff <- abs(theoretical_mean_W - mean_W)  # Difference in mean
var_diff <- abs(theoretical_var_W - var_W)  # Difference in variance
cat("Sample mean of W:", mean_W, "\n")
cat("Sample variance of W:", var_W, "\n")
cat("Difference in mean:", mean_diff, "\n")
cat("Difference in variance:", var_diff, "\n")

#######################################
# Q5: Projection Matrix and u Statistic
library(MASS)  # For mvrnorm function

# Part 1
X<-matrix(rnorm(8*5),8,5)
XtX_inv<-solve(t(X)%*%X)
PX<-X%*%XtX_inv%*%t(X)
PX2<-PX%*%PX
is_idempotent<-all.equal(PX,PX2,tolerance=1e-10)
cat("P_X:\n", PX, "\nIs idempotent? ", is_idempotent, "\n")

# Part 2
n<-5000
mu<-rep(0,8)
I<-diag(8)
Y<-mvrnorm(n,mu,I)
u<-numeric(n)
for(i in 1:n) u[i]<-t(Y[i,])%*%PX%*%Y[i,]

# Part 3
hist(u,breaks=50,main="Histogram of u",xlab="u",col="blue")
mean_u<-mean(u)
var_u<-var(u)
cat("Sample mean of u:",mean_u,"\n")
cat("Sample variance of u:",var_u,"\n")

# Part 4
theoretical_mean<-5
theoretical_var<-10
mean_diff<-abs(theoretical_mean-mean_u)
var_diff<-abs(theoretical_var-var_u)
cat("Theoretical mean:",theoretical_mean,"\n")
cat("Theoretical variance:",theoretical_var,"\n")
cat("Mean difference:",mean_diff,"\n")
cat("Variance difference:",var_diff,"\n")

#######################################
# Q6: F-Distribution
library(MASS)

# Parameters
n<-5000
mu<-rep(0,8)
I8<-diag(8)
df1<-3
df2<-5

# Generate Y
Y<-mvrnorm(n,mu,I8)

# Define X1
X1<-matrix(rnorm(8*df2),8,df2)
PX1<-X1%*%solve(t(X1)%*%X1)%*%t(X1)
PX2<-I8-PX1

# Compute f values
f_vals<-apply(Y,1,function(y) (t(y)%*%PX2%*%y/df1)/(t(y)%*%PX1%*%y/df2))

# Histogram and statistics
hist(f_vals,breaks=50,main="Histogram of F-distribution samples",xlab="f values")
mean_f<-mean(f_vals)
var_f<-var(f_vals)
cat("Sample Mean:",mean_f,"\n")
cat("Sample Variance:",var_f,"\n")

# Theoretical values
mean_theoretical<-df2/(df2-2)
var_theoretical<-(2*df2^2*(df1+df2-2))/(df1*(df2-2)^2*(df2-4))
cat("Theoretical Mean:",mean_theoretical,"\n")
cat("Theoretical Variance:",var_theoretical,"\n")
