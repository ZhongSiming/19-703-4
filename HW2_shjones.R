################################################################################
# Homework 2 Reproduction Questions 1 & 3                                      #
################################################################################
library(AER)
library(quantreg)
library(xtable)
library(knitr)
data(CPS1988)
setwd("D:/Documents/School/Carnegie Mellon/Courses/19-703 Applied Data Analysis I/Homework")
reg1.A <- rq(log(wage) ~ ethnicity + education +experience + I(experience^2), 
             data = CPS1988)
table1.A <- xtable(summary(reg1.A)$coef, auto = TRUE)
myvars <- c("Value", "t value")
table1.A <- table1.A[myvars]
colnames(table1.A) <- c("estimates", "t-values")

reg2.A <- rq(log(wage) ~ ethnicity + education + experience + I(experience^2) 
             + I(experience^3) + I(experience^4), data = CPS1988)
table2.A <- xtable(summary(reg2.A)$coef, auto = TRUE)
table2.A <- table2.A[myvars]
colnames(table2.A) <- c("estimates", "t-values")

#```{r echo = FALSE, results = 'asis'}
kable(table1.A, caption = "Table 1.A LAD Reproduction")
kable(table2.A, caption = "Tabel 2.A LAD Reproduction")
#```


################################################################################
# Homework 2 Extension Question 1                                              #
################################################################################
# Wages Histogram
png(filename = "HW2Q1wages.png", width = 2000, height = 2000, res = 200)
par(cex = 1.5, mar = c(5, 4, 1, 1), oma = c(0, 0, 0, 0))
hist(CPS1988$wage, prob = FALSE,
     breaks = "FD",
     ylim = c(0, 2000),
     xlim = c(0, 20000),
     xlab = "Weekly Wage",
     main = "Histogram of Weekly Wages",
     col = "grey")
rug(CPS1988$wage, col = rgb(1, 0, 0, .3))
rug(CPS1988$wage[CPS1988$wage > 2400], col = rgb(0, 0, 1, 1))
dev.off()

# Log Wages Histogram
png(filename = "HW2Q1logwages.png", width = 2000, height = 2000, res = 200)
par(cex = 1.5, mar = c(5, 4, 1, 1), oma = c(0, 0, 0, 0))
hist(log(CPS1988$wage),
     prob = FALSE,
     breaks = "Scott",
     ylim = c(0, 2000),
     xlab = "Log Weekly Wage",
     main = "Histogram of Log Weekly Wages",
     col = "grey")
rug(log(CPS1988$wage), col = rgb(1, 0, 0, .3))
rug(log(CPS1988$wage[CPS1988$wage > 2400]), col = rgb(0, 0, 1, 1))
dev.off()

# Education Histogram
png(filename = "HW2Q1education.png", width = 2000, height = 2000, res = 200)
par(cex = 1.5, mar = c(5, 4, 1, 1), oma = c(0, 0, 0, 0))
hist(CPS1988$education, prob = FALSE,
     breaks = "FD",
     xlab = "Years of Education",
     main = "Histogram of Years of Education",
     xlim = c(0, 20),
     col = "grey")
dev.off()

# Experience Histogram
png(filename = "HW2Q1experience.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5, mar = c(5, 4, 1, 1), oma = c(0, 0, 0, 0))
hist(CPS1988$experience, prob = FALSE,
     breaks = "FD",
     xlab = "Years of Potential Experience",
     ylim = c(0, 1000),
     xlim = c(-10, 70),
     main = "Histogram of Years of Potential Experience",
     col = "grey")
axis(side = 1, at = seq(-10, 70, by = 10))
dev.off()

################################################################################
# Homework 2 Extension Questions 2                                             #
################################################################################
# Wage vs Education
rq1 <- rq(wage ~ education, data = CPS1988)
lm1 <- lm(wage ~ education, data = CPS1988)
rq2 <- rq(log(wage) ~ education, data = CPS1988)
lm2 <- lm(log(wage) ~ education, data = CPS1988)
png(filename = "HW2Q2ScatterEd.png", width = 2000, height = 2000, res = 200)
par(cex = 1.5, mar = c(5, 4, 2, 1), mfrow = c(2,1))
plot(jitter(CPS1988$education),
     jitter(CPS1988$wage),
     ylab = "Weekly Wages",
     xlab = "Years of Education",
     pch = 19,
     cex = .5,
     col = rgb(0, 0, 0, .1),
     xlim = c(0, 20),
     ylim = c(0, 20000))
abline(rq1, col = "red", lwd = 2)
abline(lm1, col = "blue", lwd = 2)
plot(jitter(CPS1988$education),
     jitter(log(CPS1988$wage)),
     ylab = "Log Weekly Wages",
     xlab = "Years of Education",
     cex = .5,
     pch = 19,
     col = rgb(0, 0, 0, .1),
     xlim = c(0, 20))
abline(rq2, col = "red", lwd = 2)
abline(lm2, col = "blue", lwd = 2)
dev.off()

# Wage vs Experience
rq1 <- rq(wage ~ experience, data = CPS1988)
lm1 <- lm(wage ~ experience, data = CPS1988)
rq2 <- rq(log(wage) ~ experience, data = CPS1988)
lm2 <- lm(log(wage) ~ experience, data = CPS1988)
png(filename = "HW2Q2ScatterExp.png", width = 2000, height = 2000, res = 200)
par(cex = 1.5, mar = c(5, 4, 2, 1), mfrow = c(2,1))
plot(jitter(CPS1988$experience),
     jitter(CPS1988$wage),
     ylab = "Weekly Wages",
     xlab = "Potential Years of Experience",
     pch = 19,
     cex = .5,
     col = rgb(0, 0, 0, .1),
     ylim = c(0, 20000))
abline(rq1, col = "red", lwd = 2)
abline(lm1, col = "blue", lwd = 2)
plot(jitter(CPS1988$experience),
     jitter(log(CPS1988$wage)),
     ylab = "Log Weekly Wages",
     xlab = "Potential Years of Experience",
     cex = .5,
     pch = 19,
     col = rgb(0, 0, 0, .1))
abline(rq2, col = "red", lwd = 2)
abline(lm2, col = "blue", lwd = 2)
dev.off()

################################################################################
# Homework 2 Extension Questions 3                                             #
################################################################################
# Wage QQ
k <- 1:length(CPS1988$wage)
p <- (k - 0.5)/length(CPS1988$wage)
norm.q <- qnorm(p, mean(CPS1988$wage), sd(CPS1988$wage))
wages.q <- quantile(CPS1988$wage, probs = p, type = 5)
png(filename = "HW2Q3wageqq.png", width = 2000, height = 2000, res = 200)
par(cex = 1.5)
min <- min(floor(min(wages.q, norm.q)))
max <- max(ceiling(max(wages.q, norm.q)))
plot(norm.q, wages.q,
     xlim = c(min, max),
     ylim = c(min, max),
     ylab = "Quantiles of Weekly Wages",
     xlab = "Quantiles of Draws from a Normal Distribution",
     pch = 19)
abline(0, 1)
dev.off()

#Wage Log QQ
k <- 1:length(CPS1988$wage)
p <- (k - 0.5)/length(CPS1988$wage)
norm.q <- qnorm(p, mean(log(CPS1988$wage)), sd(log(CPS1988$wage)))
wages.q <- quantile(log(CPS1988$wage), probs = p, type = 5)
png(filename = "HW2Q3logwageqq.png", width = 2000, height = 2000, res = 200)
par(cex = 1.5)
min <- min(floor(min(wages.q, norm.q)))
max <- max(ceiling(max(wages.q, norm.q)))
plot(norm.q, wages.q,
     xlim = c(min, max),
     ylim = c(min, max),
     ylab = "Quantiles of Log Weekly Wages",
     xlab = "Quantiles of Draws from a Normal Distribution",
     pch = 19)
abline(0, 1)
dev.off()

################################################################################
# Homework 2 Extension Questions 4                                             #
################################################################################
reg1.A.lm <- lm(log(wage) ~ ethnicity + education + experience + I(experience^2), 
                data = CPS1988)
reg2.A.lm <- lm(log(wage) ~ ethnicity + education + experience + I(experience^2) 
                + I(experience^3) + I(experience^4), data = CPS1988)
library(car)
png(filename = "HW2Q4qqresiduals.png", width = 2000, height = 2000, res = 200)
par(mfrow = c(1, 2), cex = 1.3, mar = c(5, 4, 2, 1))
qqPlot(reg1.A.lm,
       main = "Table 1.A Regression Residuals",
       id.n = 3,
       pch = 19,
       cex = .75,
       col = rgb(0, 0, 0, .1),
       ylab = "Jackknife Residuals")
abline(0, 1)
qqPlot(reg2.A.lm,
       main = "Table 2.A Regression Residuals",
       id.n = 3,
       pch = 19,
       cex = .75,
       col = rgb(0, 0, 0, .1),
       ylab = "Jackknife Residuals")
abline(0, 1)
dev.off()
# Influence Index Plots
png(filename = "HW2Q4Influence1A.png", width = 2000, height = 2000, res = 200)
par(cex = 1.3, mar = c(5, 4, 2, 1))
influenceIndexPlot(reg1.A.lm, id.n = 10)
dev.off()
#Median Cook's D
median(cooks.distance(reg1.A.lm))
max(cooks.distance(reg1.A.lm))
max(cooks.distance(reg1.A.lm))/median(cooks.distance(reg1.A.lm))
influence(reg1.A.lm)$hat[15387]/median(influence(reg1.A.lm)$hat)
cooks.distance(reg1.A.lm)[15387]/median(cooks.distance(reg1.A.lm))
rstudent(reg1.A.lm)[15387]
CPS1988[15387, ]
CPS1988[17228, ]
# Remove outliers sensiticity analysis
reg1.A.lm.drop <- lm(log(wage) ~ ethnicity + education + experience 
                     + I(experience^2), data = CPS1988[-c(15387, 17228),])
reg2.A.lm.drop <- lm(log(wage) ~ ethnicity + education + experience 
                     + I(experience^2) + I(experience^3) + I(experience^4),
                     data = CPS1988[-c(15387, 17228),])
coefficients(reg1.A.lm)/coefficients(reg1.A.lm.drop)
coefficients(reg2.A.lm)/coefficients(reg2.A.lm.drop)

################################################################################
# Homework 2 Extension Question 5                                              #
################################################################################
png(filename = "HW2Q5backcast.png", width = 2000, height = 2000, res = 200)
par(cex = 1.3, mar = c(5, 4, 2, 1))
plot(predict(reg1.A.lm), log(CPS1988$wage),
     xlab = "Predicted Log Wages",
     ylab = "Actual Log Wages",
     xlim = c(3, 10),
     ylim = c(3, 10),
     pch = 19,
     col = rgb(0, 0, 0, .1),
     cex = 0.5)
abline(0, 1)
dev.off()
# Cross Validation
cv.fun <- function(K = 5){
  folds <- K
  ## generate vector of fold numbers
  fold.num <- rep(1:folds,
                  length.out = length(CPS1988$wage))
  fold.ran <- sample(fold.num)
  # Create data matrix
  CPS1988$exp.sq <- CPS1988$experience^2
  CPS1988$logwage <- log(CPS1988$wage)
  data1 <- CPS1988
  rmse.simple <- c()
  rmse.complex <- c()
  for(i in 1:folds){ # loop through the different folds
    # training data are those that are not in the current fold
    train <- data1[fold.ran != i, ]
    # test data are those that are in the current fold
    test <- data1[fold.ran == i, ]
    # Estimate LAD models on training data
    train1 <- rq(logwage ~ education + ethnicity + experience,
                 data = train)
    train2 <- rq(logwage ~ education + ethnicity
                 + experience + exp.sq, data = train)
    # Make predictions on test data
    test1 <- predict(train1, newdata = test)
    test2 <- predict(train2, newdata = test)
    # Calculate rMSE
    rmse.simple[i] <- sqrt(sum((test$logwage - test1)^2)/length(test$logwage))
    rmse.complex[i] <- sqrt(sum((test$logwage - test2)^2)/length(test$logwage))
  }
  return(list(rmse.simple, rmse.complex))
}
xval <- replicate(10, cv.fun())
rmse.simple <- sapply(xval[1, ], mean)
rmse.complex <- sapply(xval[2, ], mean)

mean(rmse.simple)
mean(rmse.complex)

# Quadratic vs. Quartic
cv.fun <- function(K = 5){
  folds <- K
  ## generate vector of fold numbers
  fold.num <- rep(1:folds,
                  length.out = length(CPS1988$wage))
  fold.ran <- sample(fold.num)
  # Create data matrix
  CPS1988$exp.sq <- CPS1988$experience^2
  CPS1988$exp.cube <- CPS1988$experience^3
  CPS1988$exp.quad <- CPS1988$experience^4
  CPS1988$logwage <- log(CPS1988$wage)
  data1 <- CPS1988
  rmse.simple <- c()
  rmse.complex <- c()
  for(i in 1:folds){ # loop through the different folds
    # training data are those that are not in the current fold
    train <- data1[fold.ran != i, ]
    # test data are those that are in the current fold
    test <- data1[fold.ran == i, ]
    # Estimate LAD models on training data
    train1 <- rq(logwage ~ education + ethnicity
                 + experience + exp.sq, data = train)
    train2 <- rq(logwage ~ education + ethnicity
                 + experience + exp.sq
                 + exp.cube + exp.quad, data = train)
    # Make predictions on test data
    test1 <- predict(train1, newdata = test)
    test2 <- predict(train2, newdata = test)
    # Calculate rMSE
    rmse.simple[i] <- sqrt(sum((test$logwage - test1)^2)/length(test$logwage))
    rmse.complex[i] <- sqrt(sum((test$logwage - test2)^2)/length(test$logwage))
  }
  return(list(rmse.simple, rmse.complex))
}
xval <- replicate(10, cv.fun())
rmse.simple <- sapply(xval[1, ], mean)
rmse.complex <- sapply(xval[2, ], mean)

mean(rmse.simple)
mean(rmse.complex)


################################################################################
# Homework 2 Extension Question 6                                              #
################################################################################
CPS1988$logwage <- log(CPS1988$wage)
CPS1988$exp.sq <- CPS1988$experience^2
# Estimate quadratic OLS
quad.lm <- lm(logwage ~ education + ethnicity
              + experience + exp.sq, data = CPS1988)
summary(quad.lm)
vcov1 <- vcov(quad.lm)
vars <- diag(vcov1)
beta.hat <- coefficients(quad.lm)
lower <- beta.hat - 1.96*sqrt(vars)
upper <- beta.hat + 1.96*sqrt(vars)
lower
upper
