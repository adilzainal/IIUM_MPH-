#--------------------------------------------------------
# Biostatistic practical using R Software (2025) Week 10 MPH Non parametric
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Bivariate data analysis continued
#--------------------------------------------------------
#Non parametric test (mann whitney, kruskall wallis, wilcoxon, friedman)


#mann whitney u test (non parametric for independent t-test)
library(psych)
library(purrr)
library(dplyr)
data.sav %>% 
  group_by(data.sav$sex) %>% 
  summary(data.sav$wt)
describeBy(data.sav$wt, data.sav$sex)
stat.desc(wt, sex)
attach(data.sav)
wilcox.test(wt ~ sex, data=data.sav) 

describe.by(data.sav$wt2, data.sav$sex)
wilcox.test(wt2 ~ sex, data=data.sav)

ggboxplot(data.sav, x = "sex", y = "wt2", 
          color = "sex", palette = c("#FF3300", "#E7B800"),
          order = c("Female", "Male"),
          ylab = "Weight after intervention (kg)", xlab = "Sex")

#kruskall wallis (non parametric for anova)
describeBy(data.sav$wt, data.sav$exercise)
kruskal.test(wt ~ exercise, data = data.sav)
attach(data.sav)
describe.by(data.sav$wt, data.sav$exercise)
kruskal.test(wt ~ exercise, data = data.sav)
pairwise.wilcox.test(data.sav$wt, data.sav$exercise,
                     p.adjust.method = "BH")   #post hoc test if the p value is significant

library("ggpubr")
ggboxplot(data.sav, x = "exercise", y = "wt", 
          color = "exercise", palette = c("#FF3300", "#E7B800", "#00FFFF"),
          order = c("Low", "Moderate", "High"),
          ylab = "Weight", xlab = "Exercise")

describe.by(data.sav$sbp, data.sav$exercise)
kruskal.test(sbp ~ exercise, data = data.sav)
pairwise.wilcox.test(data.sav$sbp, data.sav$exercise,
                     p.adjust.method = "BH")   #post hoc test if the p value is significant

library("ggpubr")
ggboxplot(data.sav, x = "exercise", y = "sbp", 
          color = "exercise", palette = c("#FF3300", "#E7B800", "#00FFFF"),
          order = c("Low", "Moderate", "High"),
          ylab = "Systolic", xlab = "Exercise")

#wilcoxon signed rank test (non parametric for pair t -test)
summary(data.sav$BMI)
summary(data.sav$BMI2)
wilcox.test(data.sav$BMI, data.sav$BMI2, paired=TRUE) 

#friedman test 
library(tidyverse)
library(ggpubr)
library(rstatix)
install.packages("datarium")
library(datarium)
data("selfesteem", package = "datarium")
head(selfesteem, 3)

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")

ggboxplot(selfesteem, x = "time", y = "score", add = "jitter")

res.fried <- selfesteem %>% friedman_test(score ~ time |id)
res.fried
#p<0.05 significant
