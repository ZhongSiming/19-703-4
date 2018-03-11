#### Homework 3 ####  
library(knitr)
library(kableExtra)
library(foreign)
library(broom)
library(car)
library(mgcv)
library(lmtest)
library(sandwich)

setwd("D:/Documents/School/Carnegie Mellon/Courses/19-703 Applied Data Analysis I/Homework")

#### Part 1 ####  
#### Question 1 Replicate Tables ####  
ag.data <- read.csv("ag-data.fil.csv")
#### Table 2 Regression ####  
table2.lm <- lm(log(sell) ~ drv + rec + ffin + ghw + ca + gar +reg                            + log(lot) + log(bdms) + log(fb) + log(sty), data = ag.data)
table2.tidy <- tidy(table2.lm)
table2 <- format(table2.tidy, big.mark=",", digits=3)
#kable(table2, format="latex", digits=2, align="c", caption="OLS estimation of parametric benchmark model dependent variable: log(P)", booktabs=T) %>%  
#  kable_styling(position="center")
#### Table 3 Regression ####  
table3.lm <- lm(log(sell) ~ drv + rec + ffin + ghw + ca + gar +reg + log(lot)                 + bdms + fb + sty, data = ag.data)
table3.tidy <- tidy(table3.lm)
table3 <- format(table3.tidy, big.mark=",", digits=3)
#kable(table3, format="latex", digits=2, align="c", caption="OLS estimation of  parametric model w/ discrete variables in levels: dependent variable: log(P)", booktabs=T) %>%  
#  kable_styling(position="center")

#### Question 2 Interpret Coefficients####  
summary(ag.data$lot)
summary(ag.data$bdms)
summary(ag.data$fb)
summary(ag.data$sty)
summary(ag.data$gar)

#### Part 2 ####  
#### Question 1 Histograms ####  
# Took this code from week3.r  
#### Home Prices Histogram  ####
summary(ag.data$sell)
png(filename = "HomePrices.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5)
hist(ag.data$sell, breaks = "FD" ,                                                 ylim = c(0, 110),                                                             xlab = "Home Prices",                                                         main = "Histogram of Home Prices",                                            xlim = c(0, 200000))
rug(ag.data$sell, col = rgb(0, 1, 0, 1))
rug(ag.data$sell[ag.data$sell > 81999], col = rgb(0, 0, 1, 1))
rug(ag.data$sell[ag.data$sell < 49126], col = rgb(0, 0, 1, 1))
dev.off()
# Log of Home Prices Histogram  
summary(log(ag.data$sell))
png(filename = "LogHomePrices.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5)
hist(log(ag.data$sell), breaks = "FD" ,                                           ylim = c(0, 80),                                                                xlab = "Log Home Prices",                                                       main = "Histogram of Log Home Prices",                                          xlim = c(9.5, 12.5))
rug(log(ag.data$sell), col = rgb(0, 1, 0, 1))
rug(log(ag.data$sell)[log(ag.data$sell) > 11.3], col = rgb(0, 0, 1, 1))
rug(log(ag.data$sell)[log(ag.data$sell) < 10.81], col = rgb(0, 0, 1, 1))
dev.off()
# QQplot of Log Home Prices  
# Adapted code from Section 4.1.2 of Alex Davis' Lecture Notes  
k <- dim(ag.data)[1]
p <- (1:k - 0.5)/k
norm.q <- qnorm(p, mean = mean(log(ag.data$sell)), sd = sd(log(ag.data$sell)))
prices.q <- quantile(log(ag.data$sell), p, type = 5)
png(filename = "LogHomePricesQQ.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5)
min.q <- min(floor(min(prices.q, norm.q)))
max.q <- max(ceiling(max(prices.q, norm.q)))
plot(norm.q, prices.q,                                                            xlim = c(min.q, max.q),                                                         ylim = c(min.q, max.q),                                                         ylab = "Quantiles of Log Home Prices",                                          xlab = "Quantiles of Normal Distribution",                                      main = "QQ Plot of Home Prices",                                                pch = 19,                                                                       col = rgb(0, 0, 1, .25))
abline(0, 1)
dev.off()
# Create list for Home Price Plots
sellplots <- c("HomePrices.png", "LogHomePrices.png", "LogHomePricesQQ.png")

#### Home Lot Size Histogram  ####
summary(ag.data$lot)
png(filename = "HomeLotSizes.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5)
hist(ag.data$lot, breaks = "FD" ,                                                 ylim = c(0, 100),                                                               xlab = "Home Lot Size (square feet)",                                           main = "Histogram of Home Lot Sizes",                                           xlim = c(0, 20000))
rug(ag.data$lot, col = rgb(0, 1, 0, 1))
rug(ag.data$lot[ag.data$lot > 6359], col = rgb(0, 0, 1, 1))
rug(ag.data$lot[ag.data$lot < 3601], col = rgb(0, 0, 1, 1))
dev.off()

#### Number of Bedrooms Histogram  ####
summary(ag.data$bdms)
png(filename = "HomeBedrooms.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5)
hist(ag.data$bdms, breaks = "FD" ,                                                ylim = c(0, 350),                                                               xlab = "Number of Bedrooms",                                                    main = "Histogram of Number of Bedrooms",                                       xlim = c(0, 8))
dev.off()

#### Number of Bathrooms Histogram  ####
summary(ag.data$fb)
png(filename = "HomeBathrooms.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5)
hist(ag.data$fb, breaks = "FD" ,                                                 ylim = c(0, 500),                                                              xlab = "Number of Bathrooms",                                                  main = "Histogram of Number of Bathrooms",                                     xlim = c(0, 5))
dev.off()

#### Number of Stories Histogram  ####
summary(ag.data$sty)
png(filename = "HomeStories.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5)
hist(ag.data$sty, breaks = "FD" ,                                                ylim = c(0, 250),                                                              xlab = "Number of Stories",                                                    main = "Histogram of Number of Stories",                                       xlim = c(0, 5))
dev.off()

# Create lists for other regressors
homeplots1 <- c("HomeLotSizes.png", "HomeBedrooms.png", "HomeBathrooms.png", "HomeStories.png")

#### Question 2 Tables ####
table(ag.data$drv)
table(ag.data$rec)
table(ag.data$ffin)
table(ag.data$ghw)
table(ag.data$ca)
table(ag.data$gar)
table(ag.data$reg)

#### Question 4 Jackknives ####
# Modeled off code from week67.r from class
benchmark.jk <- lm(sell ~ drv + rec + ffin + ghw + ca + gar +reg + log(lot)                       + bdms + fb + sty, data = ag.data)
png(filename = "BenchmarkJK.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5, mar = c(5, 4, 2, 1))
plot(fitted(benchmark.jk), rstudent(benchmark.jk),
     xlab = "Fitted Values",                                                         ylab = "Jackknife Residuals",                                                   main = "Benchmark Regression Jacknife Plot",                                    ylim = c(-4, 4),                                                                xlim = c(20000, 160000),                                                        pch = 19,                                                                       col = rgb(0, 0, 1, .25))
dev.off()

#### Box-Cox ####
benchmark.bc <- boxCox(ag.data$sell ~ ag.data$drv + ag.data$rec + ag.data$ffin + ag.data$ghw + ag.data$ca + ag.data$gar + ag.data$reg + log(ag.data$lot) + ag.data$bdms + ag.data$fb + ag.data$sty, family = "yjPower")
# Find the value of lambda to maximize log-likelihood
benchmark.bc$x[benchmark.bc$y == max(benchmark.bc$y)]

#### Question 5 ####
# Modeled after code in week67.r from class
cpr.lot <- lm(log(sell) ~ drv + rec + ffin + ghw + ca + gar +reg + lot                       + bdms + fb + sty, data = ag.data)
png(filename = "CPRlot.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5, mar = c(5, 4, 2, 1))
plot(ag.data$lot,
     coef(cpr.lot)["lot"]*ag.data$lot + resid(cpr.lot),
     xlab = "Lot Size (square feet)",                                                ylab = "Component + Residual",                                                  main = "Component Plus Residual for Lot Size",                                  ylim = c(-1, 1),                                                                xlim = c(0, 20000),                                                             pch = 19,                                                                       col = rgb(0, 0, 1, .25))
dev.off()

#### Box-Tidwell #####
benchmark.bt <- boxTidwell(log(sell) ~ lot, other.x = ~ drv + rec + ffin + ghw                              + ca + gar +reg + bdms + fb + sty, data = ag.data)
benchmark.bt


#### Question 6 GAM ####
benchmark.gam <- gam(log(sell) ~ drv + rec + ffin + ghw + ca + gar +reg + s(lot)                       + bdms + fb + sty, data = ag.data)
png(filename = "GAMlot.png", width = 1000, height = 1000, res = 100)
par(cex = 1.5, mar = c(5, 4, 2, 1))
plot(benchmark.gam,
     residuals = TRUE,
     shade = TRUE,
     pch = 19,
     col = rgb(0, 0, 1, .25),
     cex = .5)
dev.off()

#### Question 7 Forecasting Story ####
# Based on week4.r and week5.r code from class
benchmark <- c()
cube.root <- c()
benchmark.gam2 <- c()

ag.data$lot.cubrt <- (ag.data$lot^(-1/3)-1)/(-1/3)

forecast <- function(folds = 5){
  # Construct the folds
  fold.num <- rep(1:folds, length.out = nrow(ag.data))
  fold.ran <- sample(fold.num)
  for(i in 1:folds){
    # Construct training and test sets
    train <- ag.data[fold.ran != i, ]
    test <- ag.data[fold.ran == i, ]
    # Fit models to training data
    benchmark.lm <- lm(log(sell) ~ drv + rec + ffin + ghw + ca + gar + reg                          + log(lot) + bdms + fb + sty, data = train)
    cuberoot.lm <- lm(log(sell) ~ drv + rec + ffin + ghw + ca + gar + reg                           + lot.cubrt + bdms + fb + sty, data = train)
    gam2.lm <- gam(log(sell) ~ drv + rec + ffin + ghw + ca + gar + reg                              + s(lot) + bdms + fb + sty, data = train)
    # Test error
    benchmark.test <- (log(test$sell) - predict(benchmark.lm, newdata = test))^2
    cuberoot.test <- (log(test$sell) - predict(cuberoot.lm, newdata = test))^2
    gam2.test <- (log(test$sell) - predict(gam2.lm, newdata = test))^2
    # Store results
    benchmark <- append(benchmark, benchmark.test)
    cube.root <- append(cube.root, cuberoot.test)
    benchmark.gam2 <- append(benchmark.gam2, gam2.test)
  }
  # Test rmse
  rMSEbenchmark.lm <- sqrt(sum(benchmark)/(length(benchmark)))
  rMSEcuberoot.lm <- sqrt(sum(cube.root)/(length(cube.root)))
  rMSEgam2.lm <- sqrt(sum(benchmark.gam2)/(length(benchmark.gam2)))
  return(list(rMSEbenchmark.lm, rMSEcuberoot.lm, rMSEgam2.lm))
}

# Replicate the 5-fold cross-validation 100 times
cvs <- replicate(100, forecast())
# Pull out the average rMSE for each model
cv.benchmark <- mean(sapply(cvs[1, ], mean))
cv.cuberoot <- mean(sapply(cvs[2, ], mean))
cv.gam2 <- mean(sapply(cvs[3, ], mean))
cv.benchmark
cv.cuberoot
cv.gam2

#### Question 8 Hetero/Homoskedasticity ####
classical <- coeftest(table3.lm)
robust <- coeftest(table3.lm,
          vcov = vcovHC(table3.lm, type = "HC0"),                                         df = df.residual(table3.lm))
classical.se <- classical[ , 2]
robust.se <- robust[, 2]
ratio <- robust.se/classical.se
ratio
