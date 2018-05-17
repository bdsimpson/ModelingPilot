#library("data.table")
library("h2o")
library("splitstackshape") # used to perform stratified split of train/test

setwd("~/projects/Modeling")
load("./data/Ariel 1.Rda")


names(modeldata)[names(modeldata) == "B_Targ_Ariel_1"] <- "y"

modeldata$MATCHD_CNSMR_PRSNA_ID <- NULL
modeldata$NUM_KIDS <- NULL
modeldata$D_HR_BRD_Ariel <- NULL
modeldata$N_TENUREMONTHS <- NULL
set.seed(123)



testStrat <- stratified(modeldata, group=c("y"), size=0.6, bothSets=TRUE)

train <- testStrat$SAMP1
test <- testStrat$SAMP2


# summary(train)
# summary(test)

##  Following along with:
#     https://www.analyticsvidhya.com/blog/2016/05/h2o-data-table-build-models-large-data-sets/


# launch H2O cluster
localH2O <- h2o.init(ip="localhost",
                     nthreads = -1,       # -1 = all cores available
                     max_mem_size = '6G') 
#h2o.shutdown()

train$y <- as.factor(train$y)

library("ROSE")
train.sampled.over <- ovun.sample(y~., 
                                  data=train,
                                  method="over",  # options:  over, under, both
                                  p=0.5           # proportion of rare class
                                  )[['data']]     # only return the resulting data
train.sampled.under <- ovun.sample(y~., 
                                   data=train,
                                   method="under", # options:  over, under, both
                                   p=0.5           # proportion of rare class
                                   )[['data']]     # only return the resulting data

train.sampled.both <- ovun.sample(y~., 
                                  data=train,
                                  method="both",  # options:  over, under, both
                                  p=0.5           # proportion of rare class
                                  )[['data']]     # only return the resulting data
train.sampled.rose <- ROSE(y~.,
                           data=train,
                           p=0.5           # proportion of rare class
                           )[['data']]     # only return the resulting data

table(train.sampled.under$y)/nrow(train.sampled.under)
table(train.sampled.over$y)/nrow(train.sampled.over)
table(train.sampled.both$y)/nrow(train.sampled.both)
table(train.sampled.rose$y)/nrow(train.sampled.rose)

train.h2o.under <- as.h2o(train.sampled.under)
train.h2o.over <- as.h2o(train.sampled.over)
train.h2o.both <- as.h2o(train.sampled.both)
train.h2o.rose <- as.h2o(train.sampled.rose)
train.h2o <- as.h2o(train)


test.h2o <- as.h2o(test)

# check column names and index numbers
#colnames(train.h2o)

# assign x and y by index
y.dep <- "y" 
x.indep <- setdiff(colnames(train.h2o), y.dep) # choose all variables that are not 'y'

sampleSets <- c('train.h2o', 'train.h2o.under', 'train.h2o.over', 'train.h2o.both', 'train.h2o.rose')

#for(thisTrain in sampleSets){
#  # logistic regression
#  model.logistic <- h2o.glm(y = y.dep, x = x.indep, training_frame = get(thisTrain), validation_frame=test.h2o, family = "binomial", compute_p_values=TRUE, lambda = 0)
#  message(paste0("Sample: ",thisTrain))
#  print(h2o.auc(model.logistic, train=TRUE, valid=TRUE))
#}




############################
#############################


mtry_seq <- round(seq(from=3, to=length(x.indep), length.out = 5))
for (thisSample in sampleSets){
  #for(mtry in mtry_seq){
    ## Random Forest in H2O
    message(paste("Sampling Method:",thisSample))
    #message(paste("Mtries:",mtry))
    system.time(
      try({
      model.rforest <- h2o.randomForest(y=y.dep, 
                                        x=x.indep, 
                                        model_id = paste0("rForest_sampling_",thisSample),
                                        training_frame = get(thisSample), 
                                        validation_frame = test.h2o,
                                        nfolds = 10,
                                        fold_assignment = "Stratified",
                                        balance_classes = FALSE,  # handling with over/under sampling
                                        ntrees = 100, # default is 50, but we can safely increase when using stopping_rounds
                                        stopping_rounds = 2,  # Stop fitting new trees when the 2-tree average is within 0.001 (default value for stopping_tolerance) of the prior two 2-tree averages
                                        #mtries = mtry, 
                                        #max_depth = 4, # default is 20
                                        seed = 1122
                                        )

      }, silent=TRUE)
    )
#  }
}

rforests <- data.frame(modelName=character(), trainAUC=numeric(), testAUC=numeric())
for(thisSample in sampleSets){
  modelName <- paste0("rForest_sampling_",thisSample)
  message(paste0("Retrieving Model Name: ",modelName))
  thisAUC <- list()
  tryCatch({
    thisModel <- h2o.getModel(modelName)
    thisAUC <<- h2o.auc(thisModel, train=TRUE, valid=TRUE)
  },error=function(e){
    message('FOUND AN ERROR')
    thisAUC <<- list(train = NA, valid = NA)
  })
  rforests <- rbind(rforests, data.frame(modelName=modelName, trainAUC = thisAUC[["train"]], testAUC=thisAUC[["valid"]]))

}
rforests
h2o.auc(tmp2, train=TRUE, valid=TRUE)
tmp3 <- h2o.getModel("rForest_mtries24")

h2o.auc(tmp3, train=TRUE, valid=TRUE)

tmp <- try(h2o.getModel('rForest_mtries_44_sampling_train.h2o.over'))

## GBM in H2O


gbms <- data.frame(modelName=character(), trainAUC=numeric(), testAUC=numeric())
for(thisSample in sampleSets){
  modelName <- paste0("gbm_sampling_",thisSample)
  message(paste("Model: ",modelName))
  thisAUC <- list()
  system.time(
    model.gbm <- h2o.gbm(y=y.dep, 
                         x=x.indep, 
                         nfolds=10,
                         model_id = modelName,
                         training_frame = get(thisSample), 
                         validation_frame = test.h2o,
                         #ntrees=1000,
                         #max_depth = 4,
                         learn_rate = 0.01,
                         seed=1122)
  )
  thisModel <- h2o.getModel(modelName)
  thisAUC <- h2o.auc(thisModel, train=TRUE, valid=TRUE)
  gbms <- rbind(gbms, data.frame(modelName=modelName, trainAUC = thisAUC[["train"]], testAUC=thisAUC[["valid"]]))
}
gbms


# XGBoost
xgboosts <- data.frame(modelName=character(), trainAUC=numeric(), testAUC=numeric())
for(thisSample in sampleSets){
  modelName <- paste0("xgboost_sampling_",thisSample)
  message(paste("Model: ",modelName))
  thisAUC <- list()
  system.time(
    model.xgb <- h2o.xgboost(y=y.dep, 
                         x=x.indep, 
                         nfolds=10,
                         model_id = modelName,
                         training_frame = get(thisSample), 
                         validation_frame = test.h2o,
                         #ntrees=1000,
                         #max_depth = 4,
                         learn_rate = 0.01,
                         seed=1122)
  )
  thisModel <- h2o.getModel(modelName)
  thisAUC <- h2o.auc(thisModel, train=TRUE, valid=TRUE)
  xgboosts <- rbind(xgboosts, data.frame(modelName=modelName, trainAUC = thisAUC[["train"]], testAUC=thisAUC[["valid"]]))
}
xgboosts

# Deep Learning
dlearning <- data.frame(modelName=character(), trainAUC=numeric(), testAUC=numeric())
for(thisSample in sampleSets){
  modelName <- paste0("dlearning_sampling_",thisSample)
  message(paste("Model: ",modelName))
  thisAUC <- list()
  system.time(
    model.dlearning <- h2o.deeplearning(y=y.dep, 
                                        x=x.indep, 
                                        nfolds=10,
                                        model_id = modelName,
                                        training_frame = get(thisSample), 
                                        validation_frame = test.h2o,
                                        #epochs = 60,
                                        #hidden = c(100,100),
                                        activation = "Rectifier",
                                        seed=1122)
  )
  thisModel <- h2o.getModel(modelName)
  thisAUC <- h2o.auc(thisModel, train=TRUE, valid=TRUE)
  dlearning <- rbind(dlearning, data.frame(modelName=modelName, trainAUC = thisAUC[["train"]], testAUC=thisAUC[["valid"]]))
}
dlearning