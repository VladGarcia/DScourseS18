install.packages("MixedDataImpute") 
install.packages("mice") #Useful for imputing missing data
install.packages("stargazer") #Useful to convert tables or output to LATEX tables
library(stargazer)
library(mice)

setwd("C:\\Users\\Vladimir\\Documents\\R")
wages<- read.csv("wages.csv")
data.frame(wages)
str(wages)

#5. Drop observations where either hgc or tenure are missing 
wagesclean<-subset(wages, hgc!=0) # to drop observations where hgc are missing 
wagesclean<-subset(wagesclean, tenure!=0) # to drop observations where tenure is missing
str(wagesclean)

#6. Use stargazer to prodcue a summary table of this data frame
stargazer(wagesclean)

##(a) At what rate are log wages missing? Do you think the logwage variable is most likely to be MCAR, MAR, or MNAR?
rate.missing.logwages<- (2176-1628)/2176
rate.missing.logwages

#7. Perform the following imputation methods for missing logwage observations. For each imputation method, estimate the linear regression model

##(a) EStimate the regression model using only complete cases (do listwise deletion on the log wage variable)
logwage.clean<-subset(wagesclean, logwage!="NA")
str(logwage.clean)
linear.1<- lm(logwage ~ hgc + college + tenure + (tenure)^2 + age + married, data=wages.logwageclean)
stargazer(linear.1, title="results", align=TRUE)

##(b) Perform the mean imputation to fill in missing log wages 
wagesclean$logwage[is.na(wagesclean$logwage)]<- 1.622102
wagesclean$logwage # verify there are no NA values

##(c) Use the mice package to perform a multiple imputation regression model



