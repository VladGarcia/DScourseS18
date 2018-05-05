install.packages("sampleSelection")
library(tidyverse)
library(stargazer)
library(sampleSelection)

setwd("C:\\Users\\Vladimir\\Desktop\\Data Science")
wages<- read.csv("wages.csv")
head(wages)

# 5. Format the college, married and union variables as factors 
str(wages)
wages$college<- as.factor(as.integer(wages$college))
wages$married<- as.factor(as.integer(wages$married))
wages$union<- as.factor(as.integer(wages$union))
str(wages)

# 6. Use stargazer to produce a summary table of this data frame. Include it in your Latex writeup and discuss whether the results make sense. 
stargazer(wages)
View(wages)

# 7. Perform the following imputation methods for missing logwage observations. 
wages$exper_sqrd<- (wages$exper)^2 
attach(wages)
head(wages)
reg.results<- lm(ï..logwage ~ hgc + union + college + exper + exper_sqrd, data = wages)
summary(reg.results)

## estimate the regression using only complete cases. Do a listwise deletion on the log variable 
wages.listwise<- wages[!is.na(wages$ï..logwage),]
attach(wages.listwise)
listwise.results<- lm(ï..logwage ~ hgc + union + college + exper + exper_sqrd, data = wages)
summary(listwise.results)

## perform mean imputation to fill in missing log wages
wages$ï..logwage[is.na(wages$ï..logwage)] <- mean(wages$ï..logwage, na.rm = TRUE)
results.means<- lm(ï..logwage ~ hgc + union + college + exper + exper_sqrd, data = wages)
View(wages)

## use the sampleselection package to correct for possible non-random missingnes in the wages. 

### create a new variable called valid which flags log wage observations that are not missing
wages1<- read.csv("wages.csv")
wages1<- as.data.frame(wages1)
wages1$exper_Sqr<- (wages1$exper)^2
wages1$valid<- 1
wages1$valid[is.na(wages1$ï..logwage)] <- 0
head(wages1)
### recode the log wage variable so that invalid observatiosn are now equal to 0
wages1$ï..logwage[is.na(wages1$ï..logwage)]<- 0
### estimate the HEckman selection model by issueing the following code
heckman.results<- selection(selection = valid ~ hgc + union + college + exper + married + kids,
          outcome = ï..logwage ~ hgc + union + college + exper + I(exper_Sqr),
          data = wages1, method = "2step")
summary(heckman.results)
### use stargazer to create one regression table which has the estimate of the first three regression models 
stargazer(listwise.results, results.means, heckman.results, title = "Results", align = TRUE)

# 8. Using the same data, estimate a probit model of preferences for working in a union. To do this, use the glm function. 

probit.results<- glm(union ~ hgc + college + exper + married + kids, family = binomial(link = 'probit'), data = wages)
summary(probit.results)

# 9. Assess the impact of a counterfactual policy in which wives and motehrs are no longer allowed to work in union jobs. 

## compute predicted probabilities of the model
wages$pred.probit<- predict(probit.results, newdata = wages, type = "response")
summary(wages$pred.probit)
## change the coeffcients on married and kids to equal zero 
probit.results$coefficients["married1"] <- 0*probit.results$coefficients["married1"]
probit.results$coefficients["kids"]<- 0*probit.results$coefficients["kids"]
## compute predicted probabilities associated with the new parameter values
wages$pred.probit<- predict(probit.results, newdata = wages, type = "response")
summary(wages$pred.probit) 
## compare the average of each set of predicted probabiltiies 
# they are exactly the same. I don't know if this is due to some errors with my coding, because I am not sure this is what I was supposed to get. 
