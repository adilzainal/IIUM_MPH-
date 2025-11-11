#--------------------------------------------------------
# Biostatistic practical using R Software (2025) Week 8 MPH comparemeans
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# T-test, anova, pair t-test
#--------------------------------------------------------

#independent t-test
library(readxl)
library(ggplot2)
library(car)
library(psych)
describeBy(data.sav$sbp, data.sav$sex)
describeBy(data.sav$BMI, data.sav$smoking)
leveneTest(data = data.sav, BMI ~ smoking) # another assumption for in t test, must have equal variance, homoscedasticity
# null hypothesis, no variance diff btw male and female 
# p value not sig, do not reject null, no variance diff
t.test(data = data.sav, sbp ~ sex)
t.test(data = data.sav, sbp ~ sex, var.equal=TRUE)
t.test(data = data.sav, BMI ~ smoking, var.equal=FALSE)

#pair t-test
describe(data.sav$wt)
sd(data.sav$wt)
describe(data.sav$wt2)
sd(wt2)
t.test(wt,wt2,paired = TRUE) # p value sig, reject null, there is sig diff of wt before and after 

attach(data.sav)
describe(data.sav$BMI)
describe(data.sav$BMI2)
t.test(BMI, BMI2,paired = TRUE) 

#assumption for anova - normality, equal variance
attach(data.sav)
describeBy(data.sav$sbp,data.sav$exercise,mat=TRUE) 
leveneTest(data = data.sav, sbp ~ exercise)
fit <- aov(sbp ~ exercise, data=data.sav)
summary(fit)
TukeyHSD(fit) #post hoc test if p value is significant

describeBy(data.sav$wt2,data.sav$exercise,mat=TRUE)
leveneTest(data = data.sav, wt2 ~ exercise)
fit <- aov(wt2 ~ exercise,data=data.sav)
summary(fit)
TukeyHSD(fit) #post hoc test if p value is significant

library("ggpubr")
ggline(data.sav, x = "exercise", y = "sbp", 
       add = c("mean_se"), 
       order = c("Low", "Moderate", "High"),
       ylab = "SBP (mmHg)", xlab = "Exercise")