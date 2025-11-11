#--------------------------------------------------------
# Biostatistic practical using R Software week 10 compare prroportion (2025)
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Bivariate data analysis
#--------------------------------------------------------

# chi square test 
# fisher exact
# chi square test for trend (Cochran-Armitage trend test)
# mcnemar test
# cochran q test
# z test compare 2 proportion with 95% ci

library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  #SPSS

#chi square test for categorical variable
tbl = table(data.sav$sex, data.sav$hpt)
tbl
prop.table(tbl,2)
chisq.test(tbl) 

tbl2 = table(data.sav$BMIc, data.sav$hpt)
tbl2
prop.table(tbl2,1)
chisq.test(tbl2) 

#yates correction for correction works when at least one of the cell have less than 5 
t3 = table(data.sav$sex, data.sav$smoking)
t3
prop.table(t3, 1)
chisq.test(t3)

#fisher exact test when small sample size (when more than 20% of cells have exp frequencies less than 5)
fisher.test(data.sav$sex,data.sav$smoking)

#chi square test for trend
#Performs chi-squared test for trend in proportion. 
#This test is also known as Cochran-Armitage trend test.
# Example with default scores
smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)
prop.trend.test(smokers, patients)
chisq.test(smokers,patients) 
# Example with custom scores
prop.trend.test(smokers, patients, score = c(1, 2, 3, 4))

# Proportion of renal stone (calculi) across age
install.packages("rstatix")
library(rstatix)
xtab <- as.table(rbind(
  c(384, 536, 335),
  c(951, 869, 438)
))
dimnames(xtab) <- list(
  stone = c("yes", "no"),
  age = c("30-39", "40-49", "50-59")
)
xtabs$age <- factor(xtabs$age, levels = c("30-39", "40-49", "50-59"), ordered = TRUE)
xtab
# Compare the proportion of survived between groups
prop_trend_test(xtab)

#mcnemar test for binary paired categorical data
HPTbtw <-matrix(c(30,12,40,18),nrow = 2,
                dimnames = list("HPT before" = c("normal", "hypertension"),
                                "HPT after" = c("normal", "hypertension")))
HPTbtw
mcnemar.test(HPTbtw)

#cochran q test 
# Create a data frame
install.packages("RVAideMemoire")
library(RVAideMemoire)
data <- data.frame(
  Subject = 1:10,
  ConditionA = c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1),
  ConditionB = c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1),
  ConditionC = c(0, 1, 1, 1, 0, 0, 1, 1, 1, 0)
)
install.packages("reshape")
library(reshape)
data_long <- melt(data, id.vars = "Subject", variable.name = "Condition", value.name = "Outcome")

# Perform Cochran's Q test
cochran.qtest(value ~ variable | Subject, data = data_long)

# z test with 95% ci 
res <- prop.test(x = c(490, 400), n = c(500, 500))
# Printing the results
res 

# one sided p value
prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "greater")
prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "less")


install.packages("DescTools")
library(DescTools)
# Example data
x1 <- 80 # Number of successes in group 1
n1 <- 100 # Number of trials in group 1
x2 <- 65 # Number of successes in group 2
n2 <- 100 # Number of trials in group 2

# Calculate the confidence interval for the difference in proportions
BinomCI(x=c(42, 35, 23, 22), n=c(50, 60, 70, 80), method="clopper-pearson")

## other method = c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys",
           "modified wilson", "wilsoncc","modified jeffreys",
           "clopper-pearson", "arcsine", "logit", "witting", "pratt", 
           "midp", "lik", "blaker"),
