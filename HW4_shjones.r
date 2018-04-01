library(AER)
library(texreg)
library(mgcv)
library(ROCR)
data("SwissLabor")
setwd("D:/Documents/School/Carnegie Mellon/Courses/19-703 Applied Data Analysis I/Homework")

#### Problem 1a - Table I ####
table1 <- glm(participation ~ age + I(age^2) + education + youngkids + oldkids + income + foreign, data = SwissLabor, family = binomial(link = "probit"))
texreg(table1)

#### Problem 1b - Histograms ####
# Based on Homework 3 solution (Davis, 2018)
SwissLabor$agenorm <- SwissLabor$age*10
png(filename = "HW4P1B Histograms.png", width = 1000, height = 1000, res = 100)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(3, 2),
    cex.lab = 1.5,
    cex.axis = 1.5)
hist(SwissLabor$agenorm,
     breaks = "FD",
     xlab = "Age (Years)",
     main = "",
     xlim = c(20, 70))
rug(jitter(SwissLabor$agenorm), col = rgb(0, 1, 0, .25))
hist(SwissLabor$education,
     breaks = "FD",
     xlab = "Education (Years)",
     main = "",
     xlim = c(0, 25))
rug(jitter(SwissLabor$education), col = rgb(0, 1, 0, .25))
hist(SwissLabor$youngkids,
     xlab = "# Children Under 5",
     main = "",
     ylim = c(0, 800))
rug(jitter(SwissLabor$youngkids, amount = 0.05), col = rgb(0, 1, 0, .25))
hist(SwissLabor$oldkids,
     xlab = "# Children Over 5",
     main = "",
     ylim = c(0, 400))
rug(jitter(SwissLabor$oldkids, amount = 0.05), col = rgb(0, 1, 0, .25))
hist(SwissLabor$income,
     breaks = "FD",
     xlab = "Log Non-Labor Income",
     main = "",
     xlim = c(5, 15))
rug(jitter(SwissLabor$income), col = rgb(0, 1, 0, .25))
dev.off()

summary(SwissLabor$participation)
summary(SwissLabor$foreign)


#### Problem 3 - Logistic Regression ####
table2 <- glm(participation ~ agenorm + I(agenorm^2) + education 
              + youngkids + oldkids + income + foreign,
                data = SwissLabor, family = binomial(link = "logit"))
texreg(table2)

# Plot age
#convert non-numeric variables to binomial dummy variables
SwissLabor$biforn <- ifelse(SwissLabor$foreign == "yes", 1, 0)
SwissLabor$bipart <- ifelse(SwissLabor$participation == "yes", 1, 0)
# Re-run the regression with the dummy variable; no change from table2
table3 <- glm(bipart ~ agenorm + I(agenorm^2) + education + youngkids
                + oldkids + income + biforn,
                data = SwissLabor, family = binomial(link = "logit"))
texreg(table3)

# Create variables for mean levels of variables
meanedu <- mean(SwissLabor$education)
meanyoung <- mean(SwissLabor$youngkids)
meanold <- mean(SwissLabor$oldkids)
meanincome <- mean(SwissLabor$income)
meanforn <- mean(SwissLabor$biforn)
# Create an array with ages using the same range as the data
age <- seq(20, 60, by = .5)

# Create a plot of the coefficients vs. age based on (Davis, 2018)
png(filename = "HW4P3 Age Plot.png", width = 1000, height = 1000, res = 100)
par(cex = 1.3, mar = c(5, 4, 2, 1))
plot(jitter(SwissLabor$agenorm),
     jitter(SwissLabor$bipart, amount = 0.025),
     pch = 19,
     col = rgb(0, 1, 0, .25),
     main = "Labor Force Participation as Predicted by Model",
     ylab = "Probability of Participation",
     xlab = "Woman's Age (years)")
curve(1/(1 + exp(-(coef(table3)[1] +
                   coef(table3)[2]*x +
                   coef(table3)[3]*x^2 +
                   coef(table3)[4]*meanedu +
                   coef(table3)[5]*meanyoung +
                   coef(table3)[6]*meanold +
                   coef(table3)[7]*meanincome +
                   coef(table3)[8]*meanforn))), 
                   add = TRUE, col = rgb(0, 0, 1, 1))
dev.off()

#### Problem 4 Calibration Table ####
# Based on Week10.r (Davis, 2018)
# get the raw fitted probabilities from the first table
SwissLabor$probs <- predict(table1, type = "response")
# Get the deciles of the fitted probabilities
decile.cutpoints <- quantile(SwissLabor$probs, probs = seq(0, 1, .1))
# Create a new variable that identifies
# the quantile that each fitted probibility falls in
SwissLabor$decileID <- cut(SwissLabor$probs,
                           breaks = decile.cutpoints,
                           labels = 1:10,
                           include.lowest = TRUE)
# Calculate the number that switched in each decile
# You can see the counts by using table:
tab <- table(SwissLabor$decileID, SwissLabor$participation)
# To turn the table into a data.frame, use as.data.frame.matrix
observed <- as.data.frame.matrix(tab)
# To calculate the expected values for each decile,
# we need to sum the fitted probabilities for each decile
# We can do this by using tapply to compute
# the sum of wells$prob within each level of decileID:
expected1 <- tapply(SwissLabor$probs, SwissLabor$decileID, FUN = sum)
# Create a summary calibration table
# Create cutpoints that represent the 9 intervals that exist for deciles
interval.cutpoints <- round(quantile(SwissLabor$probs, 
                                     probs = seq(.1, 1, .1)), 2)
# Create a dataframe with these cutpoints:
cal <- data.frame(interval.cutpoints)
# Add a column of observed switches
cal$observed1 <- observed[, 2]
# Add a column of expected switches
cal$expected1 <- round(expected1, 0)
# Add columns for observed and expected non-switches:
cal$observed0 <- observed[ , 1]
cal$expected0 <-round(cal$observed1 + cal$observed0 - expected1, 0)
# Add a column for the total # of observations in each decile
cal$total <- table(SwissLabor$decileID)
cal

# Calibration Plot
# Get the observed relative frequencies
freqs <- as.numeric(cal$observed1/cal$total)
# Get the predicted relative frequencies
probs <- as.numeric(cal$expected1/cal$total)
png(filename = "HW4P4 CalibrationPlot.png", width = 1000, height = 1000,
    res = 100)
par(cex = 1.3, mar = c(5, 4, 2, 1))
plot(probs, freqs,
     pch = 19,
     col = rgb(0, 0, 1, .5),
     ylab = "Empirical Relative Frequencies",
     xlab = "Predicted Probabilities",
     xlim = c(0, 1),
     ylim = c(0, 1))
abline(0, 1)
dev.off()

#### Problem 5 GAM #####
# Based on Week67.r (Davis, 2018)
gam1 <- gam(participation ~ s(agenorm) + s(education) + youngkids
            + oldkids + income + foreign,
            data = SwissLabor, family = binomial(link = "logit"))
png(filename = "HW4P5 GAM Graph.png", width = 1000, height = 1000, 
    res = 100)
par(cex = 1.3, mar = c(5, 4, 2, 1), mfrow = c(1, 2))
plot(gam1,
     residuals = TRUE,
     shade = TRUE,
     pch = 19,
     col = rgb(0, 0, 1, .4),
     cex = .5)
dev.off()

### Problem 6 Stukel's Test ####
# Based on Week 10.r  and Section 9.5.3 (Davis, 2018)
eta.hat <- predict(table2)
positive <- ifelse(eta.hat >= 0, 1, 0)
negative <- ifelse(eta.hat < 0, 1, 0)
eta.hat.sq <- eta.hat^2

stukel1 <- glm(participation ~ agenorm + I(agenorm^2) + education 
               + youngkids + oldkids + income + foreign
                 + eta.hat.sq:positive
                 + eta.hat.sq:negative,
                 data = SwissLabor, family = binomial(link = "logit"))

logit <- glm(participation ~ agenorm + I(agenorm^2) + education + youngkids
             + oldkids + income + foreign, data = SwissLabor, 
             family = binomial(link = "logit"))

probit <- glm(participation ~ agenorm + I(agenorm^2) + education 
              + youngkids + oldkids + income + foreign,
              data = SwissLabor, family = binomial(link = "probit"))

cloglog <- glm(participation ~ agenorm + I(agenorm^2) + education 
              + youngkids + oldkids + income + foreign,
              data = SwissLabor, family = binomial(link = "cloglog"))

l <- list(stukel1, logit, probit, cloglog)
texreg(l)

### Problem 7 ROC ####
# Based on Week 10.r (Davis, 2018)
# generate empty prediction and outcome vectors
predictions1 <- c()
predictions2 <- c()
labels <- c()
# logistic regression CV function
logit.cv <- function(data1 = SwissLabor, k = 5){
  # select number of folds
  folds <- k
  # generate fold sequence
  fold.num <- rep(1:folds, length.out = nrow(data1))
  # randomize fold sequence
  fold.samp <- sample(fold.num)
  for(i in 1:k){
    # Training data
    train <- data1[fold.samp != i, ]
    # Test data takes the remaining rows
    test <- data1[fold.samp == i, ]
    # Run glm on training data
    glm1 <- glm(participation ~ agenorm + I(agenorm^2) + education 
                      + youngkids + oldkids + income + foreign,
                      data = train, family = binomial(link = "cloglog"))
    glm2 <- glm(participation ~ agenorm + education + youngkids
                      + oldkids + income + foreign,
                      data = train, family = binomial(link = "logit"))
    # Make probability predictions for test data
    glmpred1 <- predict(glm1, test, type = "response")
    glmpred2 <- predict(glm2, test, type = "response")
    # Add the predictions for this iteration to the data frame
    predictions1 <- append(predictions1, glmpred1)
    predictions2 <- append(predictions2, glmpred2)
    # Add the actual outcomes for this iteration to the data frame
    labels <- append(labels, test$participation)
  }
  return(list(predictions1, predictions2, labels))
}
cvdata1 <- replicate(100, logit.cv())
preds1 <- sapply(cvdata1[1, ], cbind)
preds2 <- sapply(cvdata1[2, ], cbind)
labs1 <- sapply(cvdata1[3, ], cbind)
library(ROCR)
# Run the ROCR prediction and performance measures
glmerr1 <- prediction(preds1, labs1)
glmperf1 <- performance(glmerr1, measure="tpr", x.measure="fpr")
glmerr2 <- prediction(preds2, labs1)
glmperf2 <- performance(glmerr2, measure="tpr", x.measure="fpr")
# This gives a vector of AUCs
glmauc1 <- performance(glmerr1, measure = "auc")
glmauc2 <- performance(glmerr2, measure = "auc")
# Unlist the AUCs
glmauc1 <- unlist(glmauc1@y.values)
glmauc2 <- unlist(glmauc2@y.values)
# Take the average
glmauc1 <- mean(glmauc1)
glmauc2 <- mean(glmauc2)
# Plot the  ROC curves:
png(filename = "HW4P7 ROC Plot.png", width = 1000, height = 1000, 
    res = 100)
par(cex = 1.3, mar = c(5, 4, 2, 1))
# ROC curve for more complex model
plot(glmperf1,
     col = "green",
     main = "Cross-Validated ROC Curves",
     avg = 'threshold',
     spread.estimate = 'stddev',
     print.cutoffs.at = seq(.0, .9, by = 0.1),
     text.adj = c(-.5, 1.2),
     xlab = "Average False Positive Rate",
     ylab = "Average True Positive Rate")
abline(0, 1)
# ROC curve for simpler model
plot(glmperf2,
     col = "blue",
     avg = 'threshold',
     spread.estimate = 'stddev',
     print.cutoffs.at = seq(.0, .9, by = 0.1),
     text.adj = c(-.5, 1.2),
     add = TRUE)
dev.off()