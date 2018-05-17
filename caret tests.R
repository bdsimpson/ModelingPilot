library("randomForest")
library("caret")
library("e1071")
library("doParallel")
library("RANN")
library("ROSE")


setwd("~/projects/Modeling")
load("./data/Ariel 1.Rda")



names(modeldata)[names(modeldata) == "B_Targ_Ariel_1"] <- "y"

modeldata$MATCHD_CNSMR_PRSNA_ID <- NULL
modeldata$NUM_KIDS <- NULL
modeldata$D_HR_BRD_Ariel <- NULL
modeldata$N_TENUREMONTHS <- NULL

## Collapse and impute gender -------------------------------
#modeldata$GENDER <- ifelse(modeldata$B_FEMALE == 1, "FEMALE",ifelse(modeldata$B_MALE == 1, "MALE",NA))
#modeldata$B_FEMALE <- NULL
#modeldata$B_MALE <- NULL
#modeldata$GenderMissing <- ifelse(is.na(modeldata$GENDER),'Y','N')
#modeldata$GENDER <- as.factor(modeldata$GENDER)
 #transform features to dummy variables
#dummy.vars <- dummyVars(~ ., data = modeldata[,names(modeldata) != 'y'])
#train.dummy <- predict(dummy.vars, modeldata[,names(modeldata) != 'y'])
#pre.process <- preProcess(train.dummy, method="knnImpute")
#imputed.data <- predict(pre.process, train.dummy)


#binaryVars <- grep('^B_', names(train.x),value=TRUE)
#for(b in binaryVars){
#  tmp <- factor(modeldata[,b]) # convert  binary variables to factors
#  levels(tmp)[levels(tmp) == "0"] <- make.names(paste(b,0,sep="-"))
#  levels(tmp)[levels(tmp) == "1"] <- make.names(paste(b,1,sep="-"))
#  modeldata[,b] <- tmp
#}
#modeldata$y <- as.factor(modeldata$y)
#levels(modeldata$y) <- list(y.0 = "0", y.1 = "1")
#str(modeldata)

table(modeldata$y)/nrow(modeldata)

#str(modeldata)

set.seed(123)

sampleRows <- createDataPartition(modeldata$y, #make distribution of y-values similar in each split
                                  times = 1, #only 1 split
                                  p=0.6,
                                  list=FALSE)

varnames <- c(names(modeldata)[names(modeldata) != 'y'])
modeldata.x <- modeldata[,varnames]
modeldata.y <- modeldata[,!names(modeldata) %in% varnames,drop=FALSE]

train.x <- modeldata.x[sampleRows,]
train.y <- modeldata.y[sampleRows,,drop=FALSE]
test.x <- modeldata.x[-sampleRows,]
test.y <- modeldata.y[-sampleRows,,drop=FALSE]

table(train.y)/nrow(train.y)
table(test.y)/nrow(test.y)

#this.formula <- as.formula(paste0("as.factor(y) ~ ",paste(varnames, collapse=" + ")))



# https://shiring.github.io/machine_learning/2017/04/02/unbalanced

#registerDoParallel(3)
# how many parallel workers?
#paste0("Workers: ",getDoParWorkers())

Sys.time()
ctrl <- trainControl(method = "repeatedcv", #repeated cross-validation
                     number = 10,  #number of folds
                     repeats = 2, # number of complete sets of folds to compute if using method="repeatedcv"
                     verboseIter = TRUE,
                     sampling = "down", #down-sampling/up-sampling.... c("none", "down", "up", "smote", "rose")
                     returnData = FALSE,
                     returnResamp = 'final',
                     allowParallel = FALSE
                     #classProbs = TRUE,
                     #summaryFunction = fivestats
                     )

model.rf <- caret::train(x=train.x, y=as.factor(train.y$y), # avoiding formula notation (author says formula notation slower)
                         data=train,
                         method="rf",
                         preProcess = c("scale", "center"),
                         tuneLength=5,
                         trControl = ctrl)
Sys.time()
pred.y <- predict(model.rf, newdata = test.x, type="prob")
RESULTS <- diagnostics(actual=as.numeric(test.y$y), predicted = pred.y[["1"]])
# save completed models
resultsFile <- paste0("caret_tests.Rda")
save(RESULTS, file=resultsFile)

