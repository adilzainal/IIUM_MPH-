#--------------------------------------------------------
# Biostatistic MPH using R Software (2025)
#--------------------------------------------------------
# Muhammad Adil ZA Lecture 5
#--------------------------------------------------------
# Getting started with R
#--------------------------------------------------------
# Learning objectives 
#1.Be familiar with reasons to use R.
#2.Download, install, and load R packages.
#3.Be able to navigate the RStudio interface including the Script, Console, Environment, Help, Files, and Plots windows.
#4.Create an R Project in RStudio using RScript and installing packages.
#5.Set a “working” directory.
#6.Send commands from the Script window to the Console in RStudio and use comments to inform scripts
#7.Create R objects and and assign values to them.
#8.Do simple arithmetic operations in R using values and objects.
#9.Call functions with arguments and change their default options.
#10.Inspect the content of vectors and manipulate their content.
#11.Subset and extract values from vectors.
#12.Correctly define and handle missing values in vectors.
#13.Use the built-in RStudio help interface
#14.Interpret the R help documentation
#15.Provide sufficient information for troubleshooting with the R user community.
#--------------------------------------------------------

# R & RStudio & RCommander

## Installation
# - R @ https://cran.r-project.org/
# - RStudio @ http://www.rstudio.com/

# RStudio Interface

## The windows arrangement
# 1. Script (Command, run, save script)
# 2. Console (Result of command, clear, terminal system shell, output)
# 3. Environment & History (Object, previous command, )
# 4. Files & others (Files directory, plots, package, help, viewer)

# - Open a new R script
#   - type all commands/functions here
#   - comments, start with "#"
#   - run all commands by Ctrl+Enter or click run

## Tasks 
# - Set the working directory (Files you want to use)
# - Install packages a.k.a libraries (Package)
#   - psych, car
install.packages("psych")  # by command
install.packages("car")

# Function, Library, Object

## Function, and functions comes with the packages that you install and put in the library.
# - function(), think of MS Excel function

## Library, download from CRAN repositories, dependencies.
library(psych)

## Help
?psych
??psych

## Object , To do useful and interesting things in R, we need to assign values to objects
# - name assigned on left side of "<-" / "="
# - variable, data (data frame, matrix, list)
# - function get more more than 1 input which is the arguement. (Example open excel dataset)
# - The return value is the output - z
# - object can be variables
x <- 1
y = 2
z = x + y
z  # type object name, you'll get the value

# Read data

## Built in data sets
data()
sleep  # view data
?sleep

## Reading data sets

# We have these files:
# - cholest.csv
# - cholest.sav
# - cholest.dta
# - cholest.xlsx
# Always make sure that you set the working directory first!
data.csv = read.csv("healthstatus.sav")  # most natural way to open data in R
library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  #SPSS
data.dta = read.dta("cholest.dta")  # STATA
library(readxl)  # library to read excel files, must install first
data.xls = read_excel("cholest.xlsx", sheet = 1)

# Handle data

## Basics
str(data.sav)  # Basic info
dim(data.sav)  # Dimension (row/case column/variable)
names(data.sav)  # Variable names

## View data
head(data.sav)  # View data, first 6 rows
tail(data.sav)  # View data, last 6 rows
data.sav  # View all
View(data.sav)  # View, graphical way

## Data structure
# vector - 1 dimension 
# matrix - 2 dimension of same type
# data frame - 2 dimension of different type

## data type 
# integer - 100
# numeric - 0.01
# character - "hello"
# logical - TRUE
# factor - "Green" "Blue"

## creating
# creating - c(), rep(), seq(), numeric()
# saving - save(), load()
# converting - as.numeric(), as.character(), as.factor(), as.logical()
# combinging c(), paste(), cbind(), rbind(), merge()

#creating variables
id <- c(1,2,3,4)
gender <- c("M","F","M","F")
age <- c(35,36,37,36)
mph_data <- data.frame(id = id, Age = age, Gender = gender)
values <- c(70,78,71,72)
mph_data$mark <- values

## The basic data types
str(data.sav)
# - numeric = numerical
# - factor = categorical
# - basically a variable in R is a VECTOR
data_num = c(1,2,3,4,5); str(data_num)
data_cat = factor( c("M", "F", "M", "F", "M") ); str(data_cat)
# - use ";" to write two lines of short commands into one

## The basic containers
# - Data frame
str(data.sav)
data.frame(data_num, data_cat)
data_frame = data.frame(data_num, data_cat); str(data_frame)
# - List
list(data_num, data_cat)
data_list = list(data_num, data_cat); str(data_list)
# - Matrix
matrix(data = c(data_num, data_cat), nrow = 5, ncol = 2)
data_matrix = matrix(data = c(data_num, data_cat), nrow = 5, ncol = 2)
data_matrix
str(data_matrix)  # shown as num

#Missing values
#R handles missing values differently than some other programs, including Stata. Missing values will appear as NA (whereas in Stata these will appear as large numeric values). Note, though, that NA is not a string, it is a symbol. If you try to conduct logical tests with NA, you are likely to get errors or NULL.

#You have several options for dealing with NA values.

#na.omit() or na.exclude() will row-wise delete missing values in your dataset
#na.fail() will keep an object only if no missing values are present
#na.action= is a common option in many functions, for example in a linear model where you might write model <- lm(y~x, data = data, na.action = na.omit).
#is.na allows you to logically test for NA values, for example when subsetting

#merging dataset
id <- c(1,2,3,4)
gender <- c("M","F","M","F")
age <- c(35,36,37,36)
drph_data <- data.frame(id = id, Age = age, Gender = gender)
values <- c(73,71,76,75)
drph_data$mark <- values

ppka_data <- merge(mph_data, drph_data, by="id")
ppka_datas <- merge(mph_data, drph_data, by="id", "gender", "age")

#long format and wideformat
long.format <- reshape(wide.format, 
                       # variable names for level one
                       varying=c("income2008", "income2009", "income2010"),
                       # new var name for long data
                       v.names="income",
                       # name for year in long data
                       timevar="year", 
                       # possible time values
                       times=c(2008, 2009, 2010), 
                       # unit-year observations
                       new.row.names=1:1000,
                       # direction to reshape
                       direction="long"
)

wide.format <- reshape(long.format,
                       # time variable
                       timevar="year",
                       # variables not to change
                       idvar="idvar", 
                       # unit-year observations
                       new.row.names=1:1000,
                       # direction of reshape
                       direction = "wide"
)



# Write to .csv
write.csv(new.data, "newdata.csv")
# Stata file
write.dta(new.data, "c:/newdata.dta")
# SPSS
write.foreign(new.data, "c:/newdata.txt", "c:/mydata.sps",   package="SPSS")
# Q&A?