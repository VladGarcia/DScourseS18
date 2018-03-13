install.packages("MixedDataImpute") 
install.packages("mice") #Useful for imputing missing data
install.packages("stargazer") #Useful to convert tables or output to LATEX tables
library(stargazer)

setwd("C:\\Users\\Vladimir\\Documents\\R")
wages<- read.csv("wages.csv")
data.frame(wages)
str(wages)

wagesclean<-subset(wages, hgc!=0) # to drop observations where hgc are missing 
str(wagesclean)
wagesclean<-subset(wagesclean, tenure!=0) # to drop observations where tenure is missing
str(wagesclean)
stargazer(wagesclean)

rate.missing.logwages<- (2176-1628)/2176
rate.missing.logwages



linear<- lm(logwage ~ hgc + college + tenure + (tenure)^2 + age + married, data = wagesclean)

stargazer(linear, title="Results", align=TRUE)
