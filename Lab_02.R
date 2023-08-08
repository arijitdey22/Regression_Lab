library(readxl)

##==================================================================================================
##Problem_01:

data <- read_xlsx("RocketData.xlsx")
dim(data)
str(data)
head(data)

model_1 <- lm(data$Y ~ data$X, data = data)
summary(model_1)

##==================================================================================================
##Problem_02:

data2 <- read_xlsx("TimeDeliveryData.xlsx")

dim(data2)
str(data2)
head(data2)

model_2 <- lm(data2$Y ~ data2$X1 + data2$X2, data = data2)
summary(model_2)

##==================================================================================================
##Problem_03:

#--- --- --- ---
#unbiased estimator of sigma^2:
X1 <- matrix(c(rep(1,nrow(data)), data$X), ncol = 2)
Y1 <- data$Y
beta1.cap <- model_1$coefficients
ue.sigma <- sqrt(as.numeric(t((Y1 - X1%*%beta1.cap)) %*% (Y1 - X1%*%beta1.cap) / (nrow(data) - 2)))

#--- --- --- ---
#value1
val2 <- as.numeric(beta1.cap[2] / (ue.sigma * sqrt(sum((X1-mean(X1))^2)) ))


##==================================================================================================
##Problem_04:

#part_(i):

n <- 5000
dat.mat <- matrix(rnorm(3*n), ncol = 3)
samp.vec <- rowSums(dat.mat^2)

#part_(ii):
library(ggplot2)

ggplot() + geom_histogram(aes(x = samp.vec, y = after_stat(density)), color = "black", fill = "darkcyan") +
  labs(y = "Density", x = "Samples")

samp.mean <- mean(samp.vec)
samp.var <- var(samp.vec)

#part_(ii):
theo.mean <- 3   #mean = df
theo.var <- 6    #var = 2 * df

#par_(iv)
abs(theo.mean - samp.mean)
abs(theo.var - samp.var)

##==================================================================================================
##Problem_05:

#part_(i)
X = matrix(rnorm(40), 8, 5)
Px = X %*% solve(t(X)%*%X) %*% t(X)

tol <- 1e-4
abs(sum(Px %*% Px - Px)) < tol

#part_(ii)
mu <- c(0,0,0,0,0,0,0,0)
sigma <- diag(8)

library(mvtnorm)
samp.Y <- rmvnorm(5000, mean = mu, sigma = sigma)

dim(Px)
dim(samp.Y)

u <- numeric()
for (i in 1:5000)
{
  Y.i <- matrix(samp.Y[i,], ncol = 1)
  u[i] <- as.numeric(t(Y.i) %*% Px %*% Y.i)
}

#part_(iii)
ggplot() + geom_histogram(aes(x = u, y = after_stat(density)), color = "black", fill = "darkcyan") +
  labs(y = "Density", x = "Samples")

samp.mean <- mean(u)
samp.var <- var(u)

#part_(iv):
theo.mean <- 5   #mean = df
theo.var <- 10    #var = 2 * df

#par_(v)
abs(theo.mean - samp.mean)
abs(theo.var - samp.var)

##==================================================================================================
##Problem_06:








































