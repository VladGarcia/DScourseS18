library(mlr)
library(glmnet)

# Load the housing data from UCI, following the examples from class
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
# add head column
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# adding 6th degree polynomials of each of the feature and 3rd degree interactions of each 
housing$lmedv <- log(housing$medv)
housing$medv  <- NULL # drop median value
formula       <- as.formula(lmedv ~ .^3 +
                              poly(crim,6) +
                              poly(zn,6) +
                              poly(indus, 6) +
                              poly(nox, 6) +
                              poly(rm, 6) +
                              poly(age, 6) +
                              poly(dis, 6) +
                              poly(rad, 6) +
                              poly(tax, 6) +
                              poly(ptratio, 6) +
                              poly(b, 6) +
                              poly(lstat, 6))
mod_matrix    <- data.frame(model.matrix(formula,housing))
# now replace the intercept column by the response since MLR will do "y ~ ." and get the intercept by default
mod_matrix[,1] = housing$lmedv
colnames(mod_matrix)[1] = "lmedv" # make sure to rename it otherwise MLR won't find it
head(mod_matrix) # make sure everything is fine

# Break up the data
n     <- nrow(mod_matrix)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
housing.train <- mod_matrix[train,]
housing.test  <- mod_matrix[test,]

## 6. Following the example from the lecture notes, estimate the LASSO model to predict log median house value, where the penalty parameter lambda is tuned by 6-fold cross validation. What is the optimal value of lambda? What is the in-sample RMSE? What is the out-of-sample RMSE (i.e. the RMSE in the test data)?

# Define the task
theTask<- makeRegrTask(id="taskname",data=housing.train,target="lmedv")
# set resampling strategy to 6-fold CV
resampleStrat<- makeResampleDesc(method="CV",iters=6)
# tell it a new prediction algorith
predAlg<- makeLearner("regr.glmnet")
# search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams<- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))
# take 50 random guesses at lambda within the inteveral specified above
tuneMethod<- makeTuneControlRandom(maxit = 50L)
# do the tuning
tunedModel<- tuneParams(learner = predAlg,
                       task = theTask,
                       resampling = resampleStrat,
                       measures = rmse,
                       par.set = modelParams,
                       control = tuneMethod,
                       show.info = TRUE)
tunedModel # optimal lambda is 0.0164, optimal alpha is 1

# apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg,par.vals = tunedModel$x)
# verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse)) # in sample error is  rmse.test.rmse=0.2019874

# train the final model
finalModel<- train(learner=predAlg, task=theTask)
# predict int test set
prediction<- predict(finalModel, newdata = housing.test)
print(head(prediction$data))
# What is the out of sample RMSE?
sqrt(mean((prediction$data$truth-prediction$data$response)^2)) # out of sample error is 0.141141

##7. REpeat the previous question, but now estimate a ridge regression model where the penalty parameter lambda is tuned by 6-fold CV. What is the optimal value of lambda now? what is the in-sample RMSE? what is the out of sample RMSE? (RMSE in the test data)

#search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams<- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))
# do the tuning again
tunedModel<- tuneParams(learner = predAlg,
                        task = theTask,
                        resampling = resampleStrat,
                        measures = rmse,
                        par.set = modelParams,
                        control = tuneMethod,
                        show.info = TRUE)
tunedModel# optimal lambda is 0.135; alpha=0

#apply the optimal algorithm parameters to the model 
predAlg<- setHyperPars(learner=predAlg, par.vals=tunedModel$x)
# verify performance on cross validated sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse)) # rmse.test.rmse=0.1670614

#train the final model
finalModel<- train(learner=predAlg,task=theTask)
#predict in test set
Prediction<- predict(finalModel,newdata = housing.test)
# what is the in-sample RMSE? What is the out of sample RMSE?
sqrt(mean((Prediction$data$truth-Prediction$data$response)^2)) # out of sample rmse is 0.1470856 

##8. Repeat the previous question, but now estimate the elastic net model. In this case, you will need to use cross validation to tune the optimal lambda and alpha (the relative weight on LASSO and ridge). What are the optimal values of l?

modelParams<- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))
# do the tuning again
tunedModel<- tuneParams(learner = predAlg,
                        task = theTask,
                        resampling = resampleStrat,
                        measures = rmse,
                        par.set = modelParams,
                        control = tuneMethod,
                        show.info = TRUE)
tunedModel # optimal lambda is 0.0113; alpha=0.129
# The value of the optimal alpha should tell you something about whether the more general elastic net is actually quite similar to either LASSO or ridge.

#apply the optimal algorithm parameters to the model 
predAlg<- setHyperPars(learner=predAlg, par.vals=tunedModel$x)
resample(predAlg,theTask,resampleStrat,measures=list(rmse))# rmse.test.rmse=0.1807778

#train the final model
finalModel<- train(learner=predAlg,task=theTask)
#predict in test set
prediction<- predict(finalModel,newdata = housing.test)
performance(prediction,measure=list(rmse)) # out of sample rmse 0.1539898
