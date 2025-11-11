#--------------------------------------------------------
# Biostatistic practical using R Software (2025) Week 11 MPH correlation
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------

#correlation 
plot(data.sav$wt, data.sav$hcy, main="Scatterplot Example", 
     xlab="Weight ", ylab="Hcy ", pch=20)

plot(data.sav$sbp, data.sav$dbp, main="Scatterplot new", 
     xlab="Systolic ", ylab="Diastolic ", pch=20)

# Simple Scatterplot
plot(data.sav$sbp, data.sav$dbp, main="Scatterplot Example", 
     xlab="SBP ", ylab="DBP ", pch=19)

# Add fit lines
abline(lm(data.sav$hcy~data.sav$wt), col="red") # regression line (y~x) 
lines(lowess(data.sav$wt,data.sav$ht), col="blue") # lowess line (x,y)


library(Hmisc)
cor.test(data.sav$hcy, data.sav$wt, method=c("pearson")) #normally distributed data
cor.test(data.sav$hcy, data.sav$wt, method=c("spearman")) #non-normally distributed data
cor.test(data.sav$sbp, data.sav$dbp, method=c("pearson")) #normally distributed data


#r <0.3 very weak, 0.3-0.5< is weak, 0.5-0.7 is moderate, >0.7 is strong
#interpret correlation coefficent = 1.-value 2.direction 3.p-value


#ordinal data using kendall tau b
# Load your data into R
# For example:
data <- data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 3, 5, 4, 6))

# Perform the Kendall's Tau-b correlation test
cor.test(data$x, data$y, method = "kendall")

# For data with ties, cor.test will automatically handle them
data_with_ties <- data.frame(x = c(1, 2, 2, 4, 5), y = c(2, 3, 5, 4, 6))
cor.test(data_with_ties$x, data_with_ties$y, method = "kendall")
