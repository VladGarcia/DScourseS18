library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(class)
install
#####################
# Read starter code #
#####################

# Read data from the UC Irvine datasets to compare the 5 Tribes in terms of their ability to classify whether someone is high-income or not. Target variable for this exercise is income$high.earner

set.seed(100)
income<- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income)<- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country","high.earner")

#####################
# Clean up the data #
#####################
# drop unnecesary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

##5. Using the mlr library, create:

#######################
# Classification task #
#######################
Thetask<- makeClassifTask(data = income.train, target = "high.earner")
print(Thetask)

####################################
# 3 fold cross validation strategy #
####################################
resampleStrat<- makeResampleDesc("CV", iters=3)

###################
# Tuning strategy #
###################
tune.method<- makeTuneControlRandom(maxit = 10L)

#########################
# Setting up algorithms #
#########################

# Trees: classif.rpart
predAlg.Trees<- makeLearner("classif.rpart", predict.type = "response")
# Logistic regression: classif.glmnet
predAlg.Logistic <- makeLearner("classif.glmnet", predict.type = "response")
# Neural network: classif.nnet
predAlg.Neural <- makeLearner("classif.nnet", predict.type = "response")
# Naive Bayes: classif.naiveBayes
predAlg.NaiveBayes <- makeLearner("classif.naiveBayes", predict.type = "response")
# kNN: classif.kknn
predAlg.kNN <- makeLearner("classif.kknn", predict.type = "response")
# SVM: classif.svm
predAlg.SVM <- makeLearner("classif.svm", predict.type = "response")

##########################
# Set up hyperparameters #
##########################

# Tree Model
Trees_Params<- makeParamSet(
  makeIntegerLearnerParam("minsplit", lower=10,upper=50),
  makeIntegerParam("minbucket",lower = 5,upper = 50),
  makeNumericParam("cp",lower = 0.001,upper = 0.2))
  
# Logit model
Logit_Params<- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),makeNumericParam("alpha",lower=0,upper=1))

# Neural network
Neural_Params<- makeParamSet(
  makeIntegerParam("size",lower=1,upper=10),
  makeNumericParam("decay", lower = 0.1,upper = 0.5),
  makeIntegerParam("maxit",lower = 1000,upper = 1000))

# Naive Bayes
# No regularization needed 

# kNN
kNN_Params<- makeParamSet(
  makeIntegerParam("k", lower = 1, upper = 30))

# SVM
SVM_PArams<- makeParamSet(
  makeDiscreteLearnerParam("cost", values = 2^c(-2,-1,0,1,2,10)),
  makeDiscreteParam("gamma", values = 2^c(-2,-1,0,1,2,10)))

###################
# Tune the models # 
###################

# Tree model
tree.tunedModel <- tuneParams(learner = predAlg.Trees,
                              task = Thetask,
                              resampling = resampleStrat,
                              measures = list(f1,gmean),
                              par.set = Trees_Params,
                              control = tune.method,
                              show.info = TRUE)
# Logit model
logit.tunedModel  <-tuneParams(learner = predAlg.Logistic,
                               task = Thetask,
                               resampling = resampleStrat,
                               measures = list(f1,gmean),
                               par.set = Logit_Params,
                               control = tune.method,
                               show.info = TRUE)
# Neural network model
neural.tunedModel <-tuneParams(learner = predAlg.Neural,
                               task = Thetask,
                               resampling = resampleStrat,
                               measures = list(f1,gmean),
                               par.set = Neural_Params,
                               control = tune.method,
                               show.info = TRUE) #having error "too many (1090) weights" ~ Error in nnet.default(x, y, w, entropy = TRUE, ...) 

# Naive Bates
  # no tuning neded
# kNN
kNN.tunedModel   <-tuneParams(learner = predAlg.kNN,
                              task = Thetask,
                              resampling = resampleStrat,
                              measures = list(f1,gmean),
                              par.set = kNN_Params,
                              control = tune.method,
                              show.info = TRUE)
# SVM
SVM.tunedModel   <-tuneParams(learner = predAlg.SVM,
                              task = Thetask,
                              resampling = resampleStrat,
                              measures = list(f1,gmean),
                              par.set = SVM_PArams,
                              control = tune.method,
                              show = TRUE)

## Apply the optimal tuning parameters
prediction.trees <- setHyperPars(learner=predAlg.Trees, par.vals = tree.tunedModel$x)
prediction.logit <- setHyperPars(learner=predAlg.Logistic, par.vals = logit.tunedModel$x)
prediction.nn    <- setHyperPars(learner=predAlg.Neural, par.vals = neural.tunedModel$x)
prediction.knn   <- setHyperPars(learner=predAlg.kNN, par.vals = kNN.tunedModel$x)
prediction.svm   <- setHyperPars(learner=predAlg.SVM, par.vals = SVM.tunedModel$x)


## verify performance
sampleResults.tree  <- resample(learner = prediction.trees, task = Thetask, resampling = resampleStrat, measures=list(gmean))

sampleResults.logit <- resample(learner = prediction.logit, task = Thetask, resampling = resampleStrat, measures=list(gmean))

sampleResults.nn    <- resample(learner = prediction.nn, task = Thetask, resampling = resampleStrat, measures=list(gmean)) # ITS HAVING ISSUES

sampleResults.knn   <- resample(learner = prediction.knn, task = Thetask, resampling = resampleStrat, measures=list(gmean))

sampleResults.svm   <- resample(learner = prediction.svm, task = Thetask, resampling = resampleStrat, measures=list(gmean))

## run model on training data
trees.finalmodel <- train(learner = prediction.trees, task = Thetask)
logit.finalmodel <- train(learner = prediction.logit, task = Thetask)
nn.finalmodel <- train(learner = prediction.nn, task = Thetask)
knn.finalmodel<- train(learner = prediction.knn, task = Thetask)
nb.finalmodel<- train(learner = predAlg.NaiveBayes, task =Thetask)
svm.finalmodel<- train(learner = prediction.svm, task = Thetask)

## predict in test set for each algorithm
trees.prediction.test <- predict(trees.finalmodel, newdata=income.test)
logit.prediction.test <- predict(logit.finalmodel, newdata=income.test)
nn.prediction.test <- predict(nn.finalmodel, newdata=income.test)
knn.prediction.test <- predict(knn.finalmodel, newdata=income.test)
nb.prediction.test<- predict(nb.finalmodel, newdata=income.test)
svm.prediction.test<- predict(svm.finalmodel, newdata=income.test)

## out of sample f1 and gmean for each algorithm
performance(trees.prediction.test, measures = list(f1,gmean))
performance(logit.prediction.test, measures = list(f1,gmean))
performance(nn.prediction.test, measures = list(f1,gmean))
performance(knn.prediction.test, measures = list(f1,gmean))
performance(nb.prediction.test, measures = list(f1,gmean))
performance(svm.prediction.test, measures = list(f1,gmean))
