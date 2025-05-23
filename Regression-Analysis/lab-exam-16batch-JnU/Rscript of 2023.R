# Question 1 data
weight <- c(165, 167, 180, 155, 212, 175, 190, 210, 200, 149, 158, 169, 170,172, 159, 168, 174, 183, 215, 195, 180, 143, 240, 235, 192, 187)
sbp <- c(130, 133, 150, 128, 151, 146, 150, 140, 148, 125, 133, 135, 150,153, 128, 132, 149, 158, 150, 163, 156, 128, 170, 165, 160, 159)
q1 <- data.frame(X= weight, Y= sbp)

#ans to the que no 1(a)
plot(q1$X,q1$Y,xlab = "Weight",ylab = "Systolic Blood Pressure",main = "Scatter plot of weight versus SBP")

#ans to the que no 1(b)
m1=lm(q1$Y~q1$X)
summary(m1)
abline(m1,col="red")

#ans to the que no 1(c)
Residuals=residuals(m1)
rounded_Residuals=round(Residuals,2)
rounded_Residuals
sum(rounded_Residuals)

#ans to the que no 1(d)
confint(m1,level=0.95)
summary(m1)$coefficients["q1$X", ]

#ans to the ques no 1(e)
ANOVA_table= anova(m1)

#ans to the ques no 1(f)
summary(m1)$r.squared

#ans to the ques no 2
clearance <- c(130, 174, 134, 191, 165, 194, 143, 186, 139, 188, 175, 156, 190, 178, 132, 148)
temperature <- c(190, 176, 205, 210, 230, 192, 220, 235, 240, 230, 200, 218, 220, 210, 208, 225)
sealed <- c(35.0, 81.7, 42.5, 98.3, 52.7, 82.0, 34.5, 95.4, 56.7, 84.4, 94.3, 44.3, 83.3, 91.4, 43.5, 51.7)
q2 <- data.frame(X1= clearance, X2= temperature,Y = sealed)
str(q2)

#ans to the que no 2(a)
X=cbind(1,q2$X1,q2$X2)
Y=q2$Y
beta_hat=solve(t(X)%*%X)%*%t(X)%*%Y
beta_hat

#ans to the que no 2(b)
m2=lm(q2$Y~q2$X1+q2$X2)
var_cov_beta_hat= vcov(m2)
var_cov_beta_hat

#ans to the que no 2(c)
H=X%*%solve(t(X)%*%X)%*%t(X)
H
I_minus_H = diag(nrow(X)) - H
I_minus_H

#ans to the que no 2(d)
summary(m2)$r.squared
summary(m2)$adj.r.squared

#ans to the que no 3
Y <- c(1, 4, 8, 9, 3, 8, 9)
X <- matrix(c(1, -1, -1, 1, 1, -1, 1, -1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 2), ncol = 3, byrow = TRUE)
C <- matrix(c(0, 1, 0, 0, 1, -1, 0, 2, -3), ncol = 3, byrow = TRUE)

m3 <- lm(Y ~ X[, 2] + X[, 3])

# Test hypothesis using car package
#install.packages("car") #install car package if it is not install yet.
library(car)
# Test β₁ = 0
linearHypothesis(m3, "X[, 2] = 0")

# Test β₁ - β₂ = 0
linearHypothesis(m3, "X[, 2] = X[, 3]")

# Test 2β₁ - 3β₂ = 0
linearHypothesis(m3, "2*X[, 2] = 3*X[, 3]")

# Question 4 data
x <- c(4, 4, 4, 5, 5, 6, 6.5, 6.5, 6.75, 7, 7.1, 7.3)
y <- c(24.60, 24.71, 23.90, 39.50, 39.60, 57.12, 67.11, 67.24, 67.15, 77.87, 80.11, 84.67)
q4 <- data.frame(X = x, Y = y)

#ans the que no 4(a)
m4=lm(q4$Y~q4$X+I((q4$X)^2))  #I(): Ensures that X^2 is treated as a squared term rather than an interaction between X and X^2 .
summary(m4)

#ans the que no 4(b)
x_factor <- factor(q4$X) # Create a factor version of X for pure error calculation
full_model <- lm(q4$Y ~ x_factor)
# ANOVA comparing polynomial to full model
anova(m4 , full_model)

#ans the que no 4(c)
m4c <- lm(q4$Y ~ poly(q4$X, degree = 2))
summary(m4c)
plot(q4$X,q4$Y)
