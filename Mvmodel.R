#**************************#
# Multivariate regression
#**************************#

data(mtcars)
str(mtcars)

mtcars$cyl.f <- factor(mtcars$cyl)

Y <- as.matrix(mtcars[, c("mpg", "disp", "hp", "wt")])
# 32*4

mvmod <- lm(Y ~ cyl.f + am + carb, data = mtcars)
str(mvmod)

mvmod$residuals

mvmod$coefficients
summary(mvmod)


#=============================#
# Sum of squares cross product
#=============================#

ybar <- colMeans(Y) # 1:4
n <- nrow(Y) # 32
m <- ncol(Y) # 4

# Ybar2 <- matrix(ybar, nrow = n, ncol = m) wrong
Ybar <- matrix(ybar, nrow = n, ncol = m, byrow = T)
# all(Ybar == Ybar2) FALSE


SSCP.T <- crossprod(Y - Ybar) # 4*4
SSCP.R <- crossprod(mvmod$fitted.values - Ybar) # 4*4
SSCP.E <- crossprod(Y - mvmod$fitted.values) # 4*4

SSCP.E
str(mvmod$fitted.values)
# num [1:32, 1:4] 21.5 21.5 28.4 20.7 16.2


all((Y - mvmod$fitted.values) == mvmod$residuals) # F
all((Y - mvmod$fitted.values) - mvmod$residuals < 0.1) #  TRUE

cov(Y - mvmod$fitted.values)
cov(Y - Ybar)
SSCP.T/n


#================================#
# Esmated error covariance matrix
#================================#

n <- nrow(Y)
p <- nrow(coef(mvmod)) - 1

SSCP.E <- crossprod(Y - mvmod$fitted.values)

SigmaHat <- SSCP.E / (n - p - 1)
SigmaTilde <- SSCP.E / n


#=======================#
# Coefficient inference
#=======================#

mvsum <- summary(mvmod) # list of 4

mvsum[[1]]

mvsum[[3]]













###################################################
# try stepwise selection 
library(dplyr)

mvmod <- list()
for(i in 0:4) {
  mvmod[[i + 1]] <- step(lm(Y ~ 1, data = select(mtcars, - cyl)),
       scope = Y ~ (cyl.f + am + carb)^2 + .,
       direction = 'forward',
       setps  = i)
}
## not applicable in mvm


## try regsubsets in leaps
library(leaps)
regfit.mv <- regsubsets(Y ~ 1 + (cyl.f + am + carb)^2 + .,
           data = select(mtcars, - cyl),
           method = 'forward',
           nvmax = 4)
## not applicable for mvm






