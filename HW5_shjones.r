#### Preliminary Setup ####
library(mlmRev)
library(texreg)
library(car)
library(HLMdiag)
library(lmtest)
library(multiwayvcov)
library(sandwich)

data("Exam", package = "mlmRev")
setwd("D:/Documents/School/Carnegie Mellon/Courses/19-703 Applied Data Analysis I/Homework")

#### P1 Histograms of variables ####
# As usual, I have liberally re-used code from (Davis, 2018)
png(filename = "HW5P1 Histograms.png", width = 1000, height = 1000, res = 100)
par(cex = 1.3, mfrow = c(2, 2), mar = c(5, 5, 2, 1),
    cex.lab = 1.5, cex.axis = 1.5)
hist(table(Exam$school),
     breaks = "FD",
     xlab = "Number of Students in School",
     main = "")
hist(Exam$normexam,
     xlab = "Score on the GCSE",
     breaks = "FD",
     main = "")
hist(unique(Exam$schavg),
     xlab = "School Average LRT Intake Score",
     breaks = "FD",
     main = "",
     xlim = c(-1, 1),
     ylim = c(0, 20))
hist(Exam$standLRT,
     xlab = "Standardized LRT Intake Score",
     breaks = "FD",
     main = "")
dev.off()

#### P1 Tables for 5 variables ####
table(Exam$schgend)
table(Exam$vr)
table(Exam$intake)
table(Exam$sex)
table(Exam$type)

#### P2 Complete, no, partial pooling ####
# Complete pooling
cp2 <- lm(normexam ~ 1, data = Exam)
# No pooling
np2 <- lm(normexam ~ factor(school) - 1, data = Exam)
# No pooling intercepts
np2.ints <- coef(np2)
# Average of the no pooling intercepts
np2.avg <- mean(np2.ints)
# Partial pooling
pp2 <- lmer(normexam ~ 1+ (1|school), data = Exam)
# Partial pooling intercepts
pp2.ints <- unlist(coef(pp2)$school)
# Average of the partial pooling intercepts
pp2.avg <- mean(pp2.ints)

#### P2 Histograms of the No pooling and partial pooling intercepts ####
png(filename = "HW5P2 NP PP Ints.png", width = 1000, height = 1000, res = 100)
par(cex = 1.3, mfrow = c(2, 1), mar = c(5, 5, 2, 1),
    cex.lab = 1.5, cex.axis = 1.5)
hist(np2.ints,
     xlab = "No Pooling Intercepts",
     breaks = "FD",
     main = "",
     xlim = c(-1.5, 1.5),
     ylim = c(0, 20))
hist(pp2.ints,
     xlab = "Partial Pooling Intercepts",
     breaks = "FD",
     main = "",
     xlim = c(-1.5, 1.5),
     ylim = c(0, 20))
dev.off()

np2.ints[5]
pp2.ints[5]


#### P4 Add intake score on the London Reading Test to regressions ####
# Complete pooling with predictor
cp4 <- lm(normexam ~ 1 + standLRT, data = Exam)
# No pooling with predictor
np4 <- lm(normexam ~ factor(school) - 1 + standLRT, data = Exam)
# No pooling intercepts
np4.ints <- coef(np4)[1:65]
# Average no pooling intercepts
np4.avg <- mean(np4.ints)
# Partial pooling with predictor
pp4 <- lmer(normexam ~ 1 + (1|school) + standLRT, data = Exam)
# Partial pooling intercepts
pp4.ints <- unlist(coef(pp4)$school[1])
# Average the partial pooling intercepts
pp4.avg <- mean(pp4.ints)

png(filename = "HW5P4 NP PP Ints.png", width = 1000, height = 1000, res = 100)
par(cex = 1.3, mfrow = c(2, 2), mar = c(5, 5, 2, 1),
    cex.lab = 1.5, cex.axis = 1.5)
hist(np2.ints,
     xlab = "No Pooling Intercepts",
     breaks = "FD",
     main = "",
     xlim = c(-1.5, 1.5),
     ylim = c(0, 25))
hist(np4.ints,
     xlab = "No Pooling Intercepts w/ Standard LRT",
     breaks = "FD",
     main = "",
     xlim = c(-1.5, 1.5),
     ylim = c(0, 25))
hist(pp2.ints,
     xlab = "Partial Pooling Intercepts",
     breaks = "FD",
     main = "",
     xlim = c(-1.5, 1.5),
     ylim = c(0, 25))
hist(pp4.ints,
     xlab = "Partial Pooling Intercepts w/ Standard LRT",
     breaks = "FD",
     main = "",
     xlim = c(-1.5, 1.5),
     ylim = c(0, 25))
dev.off()

summary(cp4)
summary(np4)
summary(pp4)


#### P5 Add the avg London Reading Test intake score for the school ####
# Partial pooling with predictors
pp5 <- lmer(normexam ~ 1 + schavg + (1|school) + standLRT, data = Exam)
# Partial pooling intercepts
pp5.ints <- coef(pp5)$school[1] + coef(pp5)$school[3]*unique(Exam$schavg)
pp5.ints <- unlist(pp5.ints)
# Average the partial pooling intercepts
pp5.avg <- mean(pp5.ints)

#### P5 Plots #####
png(filename = "HW5P5 PP with without Avg LRT.png", width = 1000, height = 1000, res = 100)
par(cex = 1.3, mfrow = c(2, 1), mar = c(5, 5, 2, 1),
    cex.lab = 1.2, cex.axis = 1.2)
plot(unique(Exam$schavg), np4.ints,
     xlab = "School Average LRT Intake Score",
     ylab = "No Pooled School Average GCSE Score",
     pch = 19,
     ylim = c(-1, 1),
     xlim = c(-1, 1))
abline(lm(np4.ints ~ unique(Exam$schavg)))
plot(unique(Exam$schavg), pp5.ints,
     xlab = "School Average LRT Intake Score",
     ylab = "Partially Pooled School Average GCSE Score",
     pch = 19,
     ylim = c(-1, 1),
     xlim = c(-1, 1))
abline(lm(pp5.ints ~ unique(Exam$schavg)))
dev.off()

summary(pp4)
summary(pp5)

#### P6 Allow standardized intake score to vary at the school level ####
pp6 <- lmer(normexam ~ 1 + standLRT + schavg + (1 + standLRT|school) ,
            data = Exam)

#### P7 Level 1 diagnostics ####
bc7 <- with(Exam, boxCox(normexam ~ factor(school)*standLRT, data = Exam, family = "yjPower"))
bc7$x[bc7$y == max(bc7$y)]
np7 <- lm(normexam ~ factor(school)*standLRT, data = Exam)
max(na.omit(cooks.distance(np7)))
median(na.omit(cooks.distance(np7)))


png(filename = "HW5P7 JK Plots.png", width = 1000, height = 1000, res= 100)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(2, 2),
    cex.lab = 1.5, cex.axis = 1.5)
plot(fitted(np7), rstudent(np7),
     xlab = "Fitted Values",
     ylab = "Jackknife Residuals",
     pch = 19,
     col = rgb(0, 0, 0, .1))
abline(h = 0, lty = 2, col = "red", lwd = 2)
lines(lowess(fitted(np7) ~ rstudent(np7)), col = "blue", lwd = 2)
plot(Exam$standLRT, rstudent(np7),
     xlab = "Standardized LRT score",
     ylab = "Jackknife LS Residuals",
     pch = 19,
     col = rgb(0, 0, 0, .1))
abline(h = 0, lty = 2, col = "red", lwd = 2)
lines(lowess(Exam$standLRT ~ rstudent(np7)), col = "blue", lwd = 2)
qqPlot(np7, id.n = 3,
       distribution = "t",
       df = df.residual(np7),
       ylab = "Jackknife LS Residuals")
dev.off()

#### P8 Level 2 diagnostics-Empirical Bayes ####
eb8 <- HLMresid(pp6, level = "school", type = "EB")
png(filename = "HW5P8 EB Hist.png", width = 1000, height = 1000, res = 100)
par(cex = 1.3, mar = c(5, 4, 2, 1), mfrow = c(2, 1),
    cex.lab = 1.3, cex.axis = 1.5)
hist(eb8[, 1],
     breaks = "FD",
     xlab = "Empirical Bayes Intercepts",
     main = "")
hist(eb8[, 2],
     breaks = "FD",
     xlab = "Emperical Bayes Slopes",
     main = "")
dev.off()

png(filename = "HW5P8 EB Resid Quantiles.png", width = 1000, height = 1000, res = 100)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(2, 2),
    cex.lab = 1.5, cex.axis = 1.5)
plot(unique(Exam$schavg), eb8[, 1],
     pch = 19,
     col = rgb(0, 0, 0, .5),
     xlab = "School-level average LRT score",
     ylab = "Empirical Bayes Intercept Residuals",
     xlim = c(-1, 1),
     ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
lines(lowess(unique(Exam$schavg), eb8[, 1]),
      col = "blue", lwd = 2)
plot(unique(Exam$schavg), eb8[, 2],
     pch = 19,
     col = rgb(0, 0, 0, .5),
     xlab = "School-level average LRT score",
     ylab = "Empirical Bayes Slope Residuals",
     xlim = c(-1, 1),
     ylim = c(-0.5, 0.5))
abline(h = 0, col = "red", lty = 2)
lines(lowess(unique(Exam$schavg), eb8[, 2]),
      col = "blue", lwd = 2)
qqPlot(eb8[, 1],
       ylab = "Empirical Bayes Intercept Residuals",
       xlab = "Normal Quantiles")
qqPlot(eb8[, 2],
       ylab = "Empirical Bayes Slope Residuals",
       xlab = "Normal Quantiles")
dev.off()

#### P9 Compare a np dummy variable w/ a pp version ####
np9 <- lm(normexam ~ standLRT + factor(school) - 1, data = Exam)
pp9 <- lmer(normexam ~ standLRT + 1 + (1 + standLRT|school),
            data = Exam)
het9 <- coeftest(np9,
                 vcov = vcovHC(np9, type = "HC0"))
clust9 <- cluster.vcov(np9,
                       Exam$school,
                       df_correction = TRUE)
coeftest(np9, vcovCL)
summary(np9)
summary(pp9)
summary(het9)
summary(clust9)

#### P10 Compare models ####
# Based on Week12.r (Davis, 2018)
simple <- c()
complex <- c()
for(i in 1:nrow(Exam)){
  # Train complex model dropping the ith observation
  exam.complex <- lmer(normexam ~ standLRT + 1 + schavg + 
                       (1 + standLRT|school), data = Exam[-i, ])
  # Find the school from the dropped observation
  school <- Exam$school[i]
  # Let's break down the prediction into each of its parts
  # There's the school intercept
  sch.int <- coef(exam.complex)$school[school, 1]
  # Plus the school-average prediction of the intercept
  avg.int <- coef(exam.complex)$school[1, 3]*Exam$schavg[i]
  # Plus the school slope
  sch.slope <- coef(exam.complex)$school[school, 2]*Exam$standLRT[i]
  # The prediction is the sum of these three components
  pred.comp <- sch.int + avg.int + sch.slope
  # Calculate the squared residual
  comp.sq.resid <- (Exam$normexam[i] - pred.comp)^2
  # Add the squared residual to the cv vector
  complex <- append(complex, comp.sq.resid)
  # Train simple model dropping the ith observation
  exam.simple <- lmer(normexam ~ standLRT + 1 + schavg + (1|school),
                      data = Exam[-i, ])
  # Let's break down the prediction into each of its parts
  # There's the school intercept
  sch.int.1 <- coef(exam.simple)$school[school, 1]
  # Plus the school-average prediction of the intercept
  avg.int.1 <- coef(exam.simple)$school[1, 3]*Exam$schavg[i]
  # Plus the overall slope
  simp.slope1 <- coef(exam.simple)$school[school, 2]*Exam$standLRT[i]
  # The prediction is the sum of these three components
  pred.simp <- sch.int.1 + avg.int.1 + simp.slope1
  # Calculate the squared residual
  simp.sq.resid <- (Exam$normexam[i] - pred.simp)^2
  # Add the squared residual to the cv vector
  simple <- append(simple, simp.sq.resid)
}
comp.rMSE <- sqrt(sum(complex)/length(complex))
simp.rMSE <- sqrt(sum(simple)/length(simple))
