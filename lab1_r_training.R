###############################################################################
## Introduction to R, Continued: What you need to know for today's exercise
## and upcoming classes
###############################################################################

## Has everyone been able to download and R?

##################
# Topics we will review from last time:
##################
# Working directories
# Installing and loading packages
# R help
# Objects, vectors and matrices
# 
##################
# New topics we will cover this time: 
##################
# Factors
# Data frames
# Converting between object types
# Helpful object commands: ls(), class(), rm()
# Missingness
# Loading data
# Looking at data
# Summarizing data
# Renaming variables
# Creating new variables and changing existing variables
# Saving data
# Linear regression
##################

##################
## Things to recall from last time:
##################
  
##################
## Working directories
##################

  # To set your working directory (don't forget to use forward slashes!):
  # (and be sure to use the correct quotation marks!) 

  setwd("/Users/sookee/Google Drive/PH250C/ph250c")
  
  # To find out what your working directory is currently set to: 
  
  getwd()


##################
## Installing and loading packages
##################

  # To install a package: 
  
  install.packages("lme4")
  
  # To call a package for use in the current R session:
  
  library(lme4)
  # or
  require(lme4)
  

##################
## R help
##################

  # To bring up the help file for a particular command or object type:
  
  help(setwd)
  ?family
  
  # Or google!

  
##################
## Object types we talked about last time
##################

  # Single numbers, strings, or booleans
  
  x <- 5 
  y <- "yellow"
  z <- TRUE
  x; y; z

  # Vectors 
  
  num.vector <- c(5, 10, 15) 
  character.vector <- c("red", "green", "blue")
  boolean.vector <- c(T, F, F) 
  num.vector; character.vector; boolean.vector
  
  # Matrices 
  
  mat <- rbind(num.vector, c(1,3,8))    # this is the 'row-bind' function
  mat
  
  # Recall how to index a matrix: 
  
  mat[1,3]
          
  
##################
## New information:
##################
  
##################
## Factors
##################

  # In addition to numerics, strings, and booleans, R also has factors. 
  # Factors are used for (usually unordered) categories and are stored as numbers 
  #  with labels. 
  # Factors can be created via the 'factor()' command: 
  
  colors <- factor(c("red", "red", "yellow", "blue", "yellow", "green"), 
                   levels=c("red","orange","yellow","green","blue","purple"))
  colors
  
  # Here, the first vector provided to the factor command gives the actual 
  #  data while the 'levels' argument tells R what groups are possible. 
  # Note that in the example above, there are groups that are possible that are 
  #  never represented in the data. 
  # In practice it is unusual to create factors  
  #  in this way and we will get to the more common way shortly. 

## Re-leveling factors
  
  # Sometimes we want to reassign or reorder the levels of a factor. 
  # This is particularly true when we want to change the reference category, which
  #   is, by default, the first level. If not specified, the levels may not have 
  #   the ordering you want:
  
  sizes <- factor(c("small", "large", "large", "small", "medium"))
  sizes
   
  # When this is the case, you have a couple options. First, you can 
  #  specify the proper order when the factor is created, as we did above.

  sizes <- factor(c("small", "large", "large", "small", "medium"))
  sizes
  sizes <- factor(c("small", "large", "large", "small", "medium"),
                  levels = c("small", "medium", "large"))
  sizes

  # Second, the levels can be specified explicitly, after the fact:

  sizes <- factor(c("small", "large", "large", "small", "medium"))
  sizes
  sizes <- factor(sizes, levels = c("small", "medium", "large"))
  sizes

  # Another way to change the order is to use relevel() to make a 
  # particular level first in the list. (This will not work for ordered factors.)

  sizes <- factor(c("small", "large", "large", "small", "medium"))
  sizes
  sizes <- relevel(sizes, "medium") # make medium first  
  sizes 

## Reassigning factor labels
  
  # Sometimes you'll also want to change the labels of the factor. If your
  # data starts as a string or numeric vector, you can replaces the values 
  # before converting it to a factor (more on this later). Or, you can 
  # rename using the levels() function:
  
  # Or by indexing a certain level in levels(): 

  sizes
  levels(sizes) <- c("baby","regular","giant")
  sizes
  
  # To rename all levels: 
  
  sizes
  levels(sizes)[3] <- "extralarge"
  sizes
  
  
  # Questions about factors?
  
  
##################
## Data frames
##################

  # Data frames are one of the most useful object types in R. 
  # They are similar to matrices in that they are essentially a collection 
  #  of vectors, but data frames allow for vectors of different data types. 
  # Data frames also allow for columns to be referred to by name, so you 
  #  can essentially reference variables within a data frame by name. 
  # This is the data type that is most similar to the type of data you 
  #  would work with in Stata or Excel.
  
  # Data frames can be created using the 'data.frame()' command: 
  
  df <- data.frame(var1 = c(1, 2, 3, 4, 5), 
                   var2 = c("one", "two", "three", "four", "five"), 
                   var3 = c(T, F, T, F, T))
  df 
  
  # Data frames can be indexed the same way that matrices can: 
  
  df[,1]            # show only the first column (all rows)
  df[2,]            # show only the second row (all columns)
  df[1,3]           # show the element in the first row and third column 
  
  # The same effect as indexing can be obtained using the 'subset()' command: 
  
  new.df <- subset(df, var1==3)
  new.df <- subset(df, var1>2 & var3==T)
  
  # Data frames can also be indexed by the column names. This can be 
  # accomplished in three ways: 
  
  df$var1           # show the column named 'var1' 
  df[,"var1"]       # show the column named 'var1' 
  df[["var1"]]      # show the column named 'var1' 

  
  # Questions about data frames?
  
  
##################
## Converting between object types
##################

  # It is often possible and useful to convert between data or object types  
  # using the 'as.something()' commands. For example, a vector of strings that
  # are actually just numbers stored as strings can be converted to a numeric 
  # vector:  
  
  nums <- c("1", "2", "3", "4", "5")
  as.numeric(nums)
  
  # Similarly, a character vector can be converted to a factor (this is 
  # usually the most convenient way to create a factor): 
  
  colors <- c("red", "yellow", "red", "blue", "red")
  as.factor(colors)
  
  # A boolean can be converted into a numeric: 
  
  bool <- c(T, T, F, F, T) 
  as.numeric(bool) 
  
  # Matrices can be converted to data frames: 
  
  as.data.frame(mat)
  
  # And data frames can be converted to matrices, provided all columns are the
  # same data type: 
  
  df <- data.frame(var1 = 1:10, var2 = 11:20, var3 = 21:30)
  as.matrix(df)
  
  # Note that for most things shown above, the opposite operation will also 
  # work (i.e. if you can convert a character vector into a factor, you can  
  # also do the reverse). 
   
  
  # Questions about converting between data types?

  
##################
## Helpful object commands: ls(), class(), rm()
##################

  # Since objects are so fundamental to how R works, there are several useful
  # commands that just give you information about objects: 
  
  ls()          # this lists all the objects that currently exist
  class(mat)    # this tells you what type of object you are working with 

  # There is also a command to remove objects if you no longer need them: 
  
  rm(df, mat)   # this removes the listed objects
  rm(list=ls()) # this removes everything, and is how you clear the workspace
  
  # It's a good idea to use this last command at the top of every script. 

  
##################
## Missingness 
##################

  # Missing values in R are generally recorded as 'NA' or '<NA>'. 
  
  # KEY IDEA: R does not consider NA to be value, so you can never 
  # test that something is missing using a 'something == NA' syntax. 
  # Instead, you must use the function'is.na()'
  
  a <- c(1, 2, NA, 4, NA, 6)
  is.na(a)      # this produces a boolean vector which tells you which elements
                # of a are missing
  sum(is.na(a)) # recall that summing a boolean vector converts TRUE's to 1's; 
                # therefore this will tell you how many missing values there
                # are in a
                
  # Sometimes we want to get rid of missing values for various reasons. This 
  # can be accomplished using the 'is.na()' function: 
  
  a[!is.na(a)]  # literally this is telling R that you want only elements of a 
                # where the element is not missing
                
  # This can also be accomplished using the 'na.omit()' function: 
  
  na.omit(a)
  
  # The 'na.omit()' function also works on data frames, but you must be careful 
  # because it will delete any row that has a missing value in any of the 
  # columns, which may or may not be what you intended
  
  # R also has an additional way of coding missing, 'NaN' which stands for 'not
  # a number'. You get this kind of missing when you perform an invalid 
  # mathematical operation: 
  
  b <- c(100/10, 100/0, log(10), log(-10), NA)
  b
  
  # 'is.na()' will be true for both 'NA' and 'NaN'. Alternatively, the function
  # 'is.nan()' will only be true for 'NaN' and not 'NA' 
  
  is.na(b)
  is.nan(b)

  # Additionally, R also understands 'NULL' which tells it that an object  
  # exists but is empty. This is not missing so much as not yet assigned. It 
  # is sometimes convenient to create an object by assigning it 'NULL' and 
  # then fill it in later
  
  c <- NULL
  c 
  
  
  # Questions on missing values?
   
  
##################
## Loading data
##################

  rm(list=ls())
  
  setwd("C:/Users/kecolson/Google Drive/courses/UCB_courses/250C_GSI/lab1")

  # Loading '.rdata' files:
  # Data that is stored in R's format--.rdata files-- is loaded using the 
  # 'load()' command. This command restores whatever objects were saved under
  # their original names. 
  
  load("lcms_1998_hhroster.rdata")
  ls()
  
  # Loading '.txt' files:
  # R also has the capacity to load data from text files using the 
  #  'read.table()' command. 
  # This automatically works on tab delimited files, and can work on other 
  #  types of files if the delimiter is specified. 
  # If your file has variable names, you need to let R know this using the  
  #  argument 'header=T'. 
  # Unlike the 'load()' command, the 'read.table()' command always produces 
  #  a data frame which will print to the screen if not assigned to an object
  
  read.table("lcms_1998_immunizations.txt", header=T)
  immun <- read.table("lcms_1998_immunizations.txt", header=T)
  nutrit <- read.table("lcms_1998_nutrition.txt", header=T, sep=";")
  ls()

  # R can also read other file formats if you load the 'foreign' package. This 
  # includes, among others, .csv files and .dta files: 
  
  library(foreign) 
  codes <- read.csv("district_and_province_codes.csv", stringsAsFactors=F) 
  edu <- read.dta("lcms_1998_education.dta", convert.underscore=T)
  ls()
    
  # Note that while 'load()' will restore any type of objects, the 
  #  'read.something()' commands can only read in spread-sheet like data and 
  #  will always create data frames. 
  
  
  # Questions on loading data?

  
##################
## Looking at data
##################

  # There are several ways to look at the data that you have in a data frame; 
  # you can use the indexing we talked about in the last session to view 
  # particular rows or columns: 

  hhroster[1:5,]                          # view the first five rows of
                                          # all variables 
  hhroster[1:10, c("hid", "pid", "sex")]  # view the first ten rows of 
                                          # of certain variables
  hhroster[1:10, 1:4]                     # view the first ten rows of 
                                          # of the first four variables
  
  # You can also use the 'head()' and 'tail()' command as a shortcut: these 
  # show the first and last 6 rows of the given data frame, respectively: 
  
  head(hhroster)
  tail(hhroster)
  
  # Questions on looking at data?
  
  fix(hroster)							# Equivalent to 'browse' in STATA
  
  
##################
## Summarizing data
##################

  # In addition to just looking at parts of the data, there are several useful
  #  ways to get summaries of the data. 
  # 'dim()' gives the dimensions of the data frame
  
  dim(edu)
  
  # The 'summary()' command prints out statistics about each variable in the 
  #  data frame. The type of statistics are dependent on the type of each 
  #  variable: 
  
  summary(edu)          # summary for all variables
  summary(edu$weight)   # summary for just 'weights' 
  
  summary(edu$weight[edu$attend == "no"])  # indexing/ subsetting for selected values
  
  # The 'table()' command can be used to give tabulations of a single variable
  # or cross-tabulations of two variables: 
  
  table(edu$attend)
  table(edu$type.school, edu$attend)
  
  # 'table()' will tell you all the unique values of a variable, along with the
  #  tabulation. 
  # The 'unique()' command also tells you the unique values of a variable but 
  #  skips the tabulation. This is also a helpful way to find out 
  #  the number of unique values: 
  
  unique(edu$reason.leave)
  length(unique(edu$reason.leave))
      

  # Questions on summarizing data?
  
  
##################
## Renaming variables
##################

  # The names of variables or columns in a data frame are visible whenever you
  # print part of data frame to the screen in any way: 
  
  head(nutrit)
  
  # It is also possible to get the variable names using the 'names()' command: 
  
  names(nutrit)
  
  # The 'names()' command produces a character vector containing a string for
  #  each name. 
  # We can rename all of the variables by replacing this vector with 
  #  a new vector, or we can rename just certain elements by replacing only 
  #  parts of this vector: 
  
  names(nutrit) <- c("hid", "pid", "weight", "mother.edu", "day", "month", 
                     "year", "c.weight", "c.height")
  head(nutrit)
  names(nutrit)[1] <- "household.id"
  names(nutrit)[2] <- "person.id"
  head(nutrit)


  # Questions on renaming variables?
  
  
##################
## Creating new variables and changing existing variables 
##################

  # If we want to create a new variable in an existing data frame we simply 
  # use one of the assignment operators ('<-', '=') to assign something to 
  # a new variable name: 
  
  immun$total.immunizations <- immun$bcg + immun$dpt + immun$polio + immun$measles
  head(immun)
  
  # We can modify an existing variable the same way, by simply using one of 
  # the assignment operators to assign new values to an existing variable: 
  
  hhroster$province <- 10*hhroster$province 
  hhroster[1:6,1:4]
  hhroster$province <- hhroster$province/10
  hhroster[1:6,1:4]
  
  # It is also possible to modify parts of an existing variable using 
  # the same types of indexing we have been using 
  
  hhroster$sex[hhroster$sex == "M"] <- "male" 
  hhroster[1:6,c("province","district","sex")]
  hhroster$sex[hhroster$sex == "F"] <- "female" 
  hhroster[1:6,c("province","district","sex")]
  
  # Finally, it is often helpful to take a continuous variable and categorize it. 
  # This can be done using the function cut():
  
  hhroster$age.cat <- cut(hhroster$age, breaks=c(0,15,30,45,60,75,99))
  table(hhroster$age, hhroster$age.cat)
  
  hhroster$age.cat <- cut(hhroster$age, c(0,15,30,45,60,75,99), right = FALSE)
  table(hhroster$age, hhroster$age.cat)

  
  # Questions on creating new variables or changing existing variables?
  
  
##################
## Saving data
##################

  # Saving data proceeds exactly in the reverse of loading data. The 'save()' 
  # function allows you to save objects (as many as you want and any type) as a
  # .rdata file while 'write.table()', 'write.dta()', and 'write.csv()' allow
  # you to save a single data frame to a text file, stata file, and comma 
  # separated values file, respectively. In all cases you must specify the 
  # object(s) you are saving and the name of the file you want to save to. 
  # Additionally, I usually find it helpful when writing .txt and .csv files to
  # tell R not to write the row names by using 'row.names=F': 

  save(immun, hhroster, file="new_data.rdata")
  write.table(immun, file="new_immun.txt", row.names=F)
  write.dta(hhroster, file="new_hhroster.dta")
  write.csv(immun, file="new_immun.csv", row.names=F)
  
  
  # Questions on saving data?


##################
## Linear regression
##################

  # Standard linear regression is handled in R using the linear models command: 'lm()'.
  
  # To use 'lm()' you must specify a model formula and then saywhere the data 
  # is located. 
  
  # A model formula in R has the dependent (response) variable on the left of 
  # a '~' sign and then lists the independent variables on the right side. R 
  # assumes it should include an intercept and there is no need to specify an 
  # intercept term in the model formula. 
  
  # In the code below we are asking R to fit an OLS regression of: 
  #     hhsize = B0 + B1*age + B3*sex + B4*age*sex + e 
  
  # By specifying that the data is the data frame 'hhroster', R knows to look 
  #  for 'sex' and 'age' in that data frame. 
  # When we fit the model we are storing all of the output in the object 'mod'. 
  
  mod <- lm(hhsize ~ age + factor(sex) + age*factor(sex), data=hhroster)
  
  # If we just type in 'mod' after the fact, we get a short summary of the model 
  # that gives the estimates of the coefficients:
  
  mod
  
  # If we use the summary() command with the object 'mod' we get a more in-depth 
  # summary that includes information about residuals and estimates, standard 
  # errors, and p-values for the coefficients as well as R^2 and an F-statistic 
  # for the model. 
  
  summary(mod)
  
## Extracting results ## 

  # The object where we store the results of an 'lm()' command (in the above 
  # example, 'mod') is an 'lm' object, a type of model object. Model objects
  # often have multiple elements. You can find out what the 
  # elements of most model object are using the 'names()' command: 
  
  names(mod)
  
  # If there is then some particular information from this list that you want
  # to view or extract, you can do so using indexing like this: 
  
  mod$coefficients      # the estimated coefficients
  # or 
  mod[["coefficients"]]
  
  # You can also obtain a description of what each of these items is by 
  # looking at the 'value' section in the help file for 'lm()' 
  
  help(lm)
  
  # There are also a series of commands that do useful things specifically 
  # with model objects. Often, they simply extract information stored in the
  # model object: the commands 'fitted()' and 'residuals()' will create a 
  # vector of the fitted values and the residuals, respectively, corresponding 
  # to each observation you originally provided the 'lm()' command: 
  
  fitted.values <- fitted(mod)  
  residuals <- resid(mod)

  # Similarly the 'coef()' command can be used to extract the coefficients. 

  coef(mod)
  
  # The command 'confint()' actually does some calculations on the model 
  # object and provides confidence intervals for the estimated 
  # coefficients. 
  
  confint(mod)
  
  # Note that you can also run statistical tests such as ANOVA tests on model objects
  # using syntax similar to relevant.function(mod). We will see this in upcoming lectures.

## Making predictions ##
  
  # The 'predict()' command, by default, simply provides fitted values. 
  # However, if you provide the 'predict()' command with a new data frame that
  # contains all of the independent variables from your model, you can actually 
  # do out-of-sample predictions for your model, based on the provided new
  # data: 

  head(fitted(mod))
  predictions <- predict(mod)
  head(predictions)
  
  newdata <- hhroster
  newdata$age[newdata$age>50] <- 50
  predictions <- predict(mod, newdata=newdata)
  head(predictions)
  
  
## Important topics we didn't cover!
  
  # Lists
  # Graphics
  # Merging data
  # Many others!
  
    
# END