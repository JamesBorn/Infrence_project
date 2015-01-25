#The goal of this second part of project is to analyze the ToothGrowth data in the R datasets package through the following steps:
#Load the ToothGrowth data and perform some basic exploratory data analyses
#Provide a basic summary of the data.
#Use confidence intervals and/or hypothesis tests
#to compare tooth growth by supp and dose. 
#(Only use the techniques from class, even if 
#there's other approaches worth considering)
#State your conclusions and the assumptions needed for your conclusions

#Load the ToothGrowth data and perform some basic exploratory data analyses
   
#Using summary statistics to spot problems by using DescTools Packages
#tools for descriptive statistics


library(datasets)
data(ToothGrowth)
library(DescTools)
Desc(ToothGrowth)

 
 
#The variable dose should be a factor, not numeric we have to assign 
#it to be a factor

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#look at the dataset variables after conversion
 
 
Desc(ToothGrowth)
#Provide a basic summary of the data

table(ToothGrowth$dose, ToothGrowth$supp)

library(ggplot2)
ggplot(aes(x=dose, y=len), data=ToothGrowth) + geom_boxplot(aes(fill=dose))




ggplot(aes(x=supp, y=len), data=ToothGrowth) + geom_boxplot(aes(fill=supp))

# Use confidence intervals and/or hypothesis tests

#control of the differences between groups assuming unequal inter groups variances
  
t.test(len ~ supp, data = ToothGrowth)


# control of the differences between all subset groups  
ToothGrowth.doses1 <- subset (ToothGrowth, dose %in% c(0.5, 1.0)) 
ToothGrowth.doses2 <- subset (ToothGrowth, dose %in% c(0.5, 2.0)) 
ToothGrowth.doses3 <- subset (ToothGrowth, dose %in% c(1.0, 2.0))

t.test(len ~ dose, data = ToothGrowth.doses1)
t.test(len ~ dose, data = ToothGrowth.doses2)
t.test(len ~ dose, data = ToothGrowth.doses3)


