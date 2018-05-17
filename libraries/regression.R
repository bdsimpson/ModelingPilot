#################################################################
#  regression.R
#
#  This script brings in required libraries and defines functions
#     for regression models
#  - Ultimately these will be hidden in a package
#
#  Includes:
#   1) fullLogistic()
#   2) forwardLogistic()
#   3) forwardLogisticOld() -- keeping for reference
#   4) lassoRegression()
#   5) ridgeRegression()
#   6) elasticNetRegression()
#   7) randomForest()
#
#  Each Regression function expects the following inputs:
#    train   - data frame containing training data
#    test    - data frame containing testing data
#
#   * these data frames should have the following characteristics:
#     - Dependent variable named 'y'
#     - No missing values
#
#  Each Regression function should return a list object containing the following:
#    model           - full regression results
#    aic             - Calculated Akaike Information Criterion (NULL for glmnet models)
#    formula         - Formula object for result model (when applicable)
#    coefficients    - data frame containing selected variables and their coefficients
#    train.predicted - vector of y-hat values for the training dataset
#    test.predicted  - vector of y-hat values for the testing dataset
#    warnings        - collection of any warnings encountered during regression
#    errors          - collection of any errors encountered during regression
#
#################################################################


if(!require(logistf)){  # might be able to omit this one if not using forwardLogisticOld
  install.packages("logistf")
  library(logistf)
}
if(!require(brglm)){
  install.packages("brglm")
  library(brglm)
}
if(!require(glmnet)){
  install.packages("glmnet")
  library(glmnet)
}
if(!require(mltools)){
  install.packages("mltools")
  library(mltools)
}
if(!require(bigstep)){
  install.packages("bigstep")
  library(bigstep)
}
if(!require(ROSE)){
  install.packages("ROSE")
  library(ROSE)
}
if(!require(h2o)){
  install.packages("h2o")
  library(h2o)
}
H2O_OPTS <- list(ip="localhost", nthreads = -1, min_mem_size='4G', max_mem_size='8G')
if(!require(stringi)){
  install.packages("stringi")  # using to generate random strings to help with H2O table names
  library(stringi)
}

if(!require(splitstackshape)){
  install.packages("splitstackshape")
  library(splitstackshape)
}




fullLogistic <- function(train, test){
  #run full logistic regression with brglm
  warn <- err <- NULL
  train.model <- withCallingHandlers(brglm(y ~ ., family=binomial(link="logit"), data=train, pl=FALSE),
                                     error = function(e){
                                       err <<- conditionMessage(e)
                                       NULL
                                     },
                                     warning = function(w){
                                       warn <<- append(warn, conditionMessage(w))
                                       #return(res)
                                       invokeRestart("muffleWarning")
                                     })
  if(!is.null(err)) return(list(model=NULL,aic=NULL,formula=NULL,train.predicted=NULL,test.predicted=NULL,warnings=unique(warn),errors=unique(err)))
  coefficients <- data.frame(summary(train.model)$coefficients, check.names=FALSE)
  # split out the intercept
  intercept <- coefficients["(Intercept)",]
  coefficients <- coefficients[row.names(coefficients) != "(Intercept)",]
  # order coefficients by z-score
  coefficients <- coefficients[order(-coefficients$`z value`),]
  # add the intercept back in to the top of coefficients
  coefficients <- rbind(intercept, coefficients)
  coefficients <- tibble::rownames_to_column(coefficients, var="variable")
  response <- list(#model =train.model,
                   aic = train.model$aic,
                   formula = as.formula(paste("Formula: y ~",paste0(names(train.model$coefficients),collapse=" + "))),
                   coefficients = coefficients,
                   train.predicted = predict(train.model, type="response"),
                   test.predicted = predict(train.model, newdata = test, type="response"),
                   warnings = unique(warn),
                   errors = unique(err)
  )
  return(response)
} # end of fullLogistic()




##############################################################
## Faster forward logistic Regression using bigstep
#   -- bigstep is significantly faster on large datasets
#   -- Performs initial forwards/backwards sweeps using single-variable tests
#   -- Takes resulting model and begins stepwise regression based on p-value
forwardLogistic <- function(train, test, pvalue=0.05){
  warn <- err <- NULL
  train_matrix <- model.matrix(~., train[, colnames(train) != 'y', with=FALSE]) # matrix formula (~.) and independent vars
  test_matrix <- model.matrix(~., test[, colnames(test) != 'y', with=FALSE])
  # remove intercept added by model.matrix
  train_matrix <- train_matrix[, colnames(train_matrix) != '(Intercept)']
  test_matrix <- test_matrix[, colnames(test_matrix) != '(Intercept)']
  train.model <- withCallingHandlers({
                      tmp1 <- selectModel(X=train_matrix, y=train$y, fitFun=fitLogistic, p=ncol(train_matrix), minpv = pvalue)
                      brglm(as.formula(paste0('y ~ ',paste(tmp1, collapse="+"))), family=binomial(link="logit"), data=train)
                      
                    },
                    error = function(e){
                      err <<- conditionMessage(e)
                      NULL
                    },
                    warning = function(w){
                      warn <<- append(warn, conditionMessage(w))
                      #return(res)
                      invokeRestart("muffleWarning")
                    })
  coefficients <- data.frame(summary(train.model)$coefficients, check.names=FALSE)
  # split out the intercept
  intercept <- coefficients["(Intercept)",]
  coefficients <- coefficients[row.names(coefficients) != "(Intercept)",]
  # order coefficients by z-score
  coefficients <- coefficients[order(-coefficients$`z value`),]
  # add the intercept back in to the top of coefficients
  coefficients <- rbind(intercept, coefficients)
  coefficients <- tibble::rownames_to_column(coefficients, var="variable")
  response <- list(#model =train.model,
                   aic = train.model$aic,
                   formula = as.formula(paste("y ~",paste0(names(train.model$coefficients),collapse=" + "))),
                   coefficients = coefficients,
                   train.predicted = predict(train.model, type="response"),
                   test.predicted = predict(train.model, newdata = test, type="response"),
                   warnings = unique(warn),
                   errors = unique(err)
  )  
  return(response)
} # End of forwardLogistic()







##############################################################
## Forward Logistic Regression (Slow) (Standard Method with brglm and logistf) -------------------------------
forwardLogisticOld <- function(train, test){
  warn <- err <- NULL
  null.model <- logistf(y ~ 1, data=train, pl=FALSE)
  train.logistf <- forward(null.model, data = train, pl=FALSE)
  train.model <- withCallingHandlers(brglm(train.logistf$formula, family=binomial(link="logit"), data=train),
                                     error = function(e){
                                       err <<- conditionMessage(e)
                                       NULL
                                     },
                                     warning = function(w){
                                       warn <<- append(warn, conditionMessage(w))
                                       #return(res)
                                       invokeRestart("muffleWarning")
                                     }) 
  coefficients <- data.frame(summary(train.model)$coefficients, check.names=FALSE)
  # split out the intercept
  intercept <- coefficients["(Intercept)",]
  coefficients <- coefficients[row.names(coefficients) != "(Intercept)",]
  # order coefficients by z-score
  coefficients <- coefficients[order(-coefficients$`z value`),]
  # add the intercept back in to the top of coefficients
  coefficients <- rbind(intercept, coefficients)
  coefficients <- tibble::rownames_to_column(coefficients, var="variable")
  response <- list(#model =train.model,
                   aic = train.model$aic,
                   formula = as.formula(paste("y ~",paste0(names(train.model$coefficients),collapse=" + "))),
                   coefficients = coefficients,
                   train.predicted = predict(train.model, type="response"),
                   test.predicted = predict(train.model, newdata = test, type="response"),
                   warnings = unique(warn),
                   errors = unique(err)
  )  
  return(response)
}#end of forwardLogistic()

###############################################################
## LASSO Regression -----------------------
lassoRegression <- function(train, test){
  warn <- err <- NULL
  
  train_sparse <- sparse.model.matrix(~., train[, colnames(train) != 'y', with=FALSE]) # matrix formula (~.) and independent vars
  test_sparse <- sparse.model.matrix(~., test[, colnames(test) != 'y', with=FALSE])
  # remove intercept added by model.matrix
  train_sparse <- train_sparse[, colnames(train_sparse) != '(Intercept)']
  test_sparse <- test_sparse[, colnames(test_sparse) != '(Intercept)']
  
  ################################
  ## alpha = 1 for LASSO ---------
  
  # use cross-validation to find the ideal lambda to use
  train.cv <- withCallingHandlers(cv.glmnet(y=train$y, x=train_sparse, family="binomial", nfolds = 10, type.measure="deviance", parallel = FALSE, alpha = 1, standardize = TRUE),
                                     error = function(e){
                                       err <<- conditionMessage(e)
                                       NULL
                                     },
                                     warning = function(w){
                                       warn <<- append(warn, conditionMessage(w))
                                       #return(res)
                                       invokeRestart("muffleWarning")
                                     }) 
  print(paste0("lambda selected: ",train.cv$lambda.1se))
  #use the lambda.lse to fit at 1 standard-error from least squared error
  #letting glmnet standardize because it transforms the coefficients back automatically
  train.model <- withCallingHandlers(glmnet(y=train$y, x=train_sparse, family="binomial", lambda = train.cv$lambda.1se, alpha = 1, standardize = TRUE),
                                     error = function(e){
                                       err <<- conditionMessage(e)
                                       NULL
                                     },
                                     warning = function(w){
                                       warn <<- append(warn, conditionMessage(w))
                                       #return(res)
                                       invokeRestart("muffleWarning")
                                     }) 
  tmp_coeffs <- coef(train.model)
  coefficients <- data.frame(variable = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], Estimate = tmp_coeffs@x)
  
  # split out the intercept
  intercept <- coefficients[coefficients$variable == "(Intercept)",]
  coefficients <- coefficients[coefficients$variable != "(Intercept)",]
  # order coefficients by beta estimate
  coefficients <- coefficients[order(-coefficients$Estimate),]
  # add the intercept back in to the top of coefficients
  coefficients <- rbind(intercept, coefficients)
  response <- list(#model =train.model,
                   aic = NULL,
                   formula = as.formula(paste("y ~",paste0(coefficients$variable,collapse=" + "))),
                   coefficients = coefficients,
                   train.predicted = as.vector(predict(train.model, newx = train_sparse, s=train.cv$lambda.1se, type="response")),
                   test.predicted = as.vector(predict(train.model, newx = test_sparse, s=train.cv$lambda.1se, type="response")),
                   warnings = unique(warn),
                   errors = unique(err)
  )
  
  return(response)
} # end of lassoRegression


###############################################################
## RIDGE Regression -----------------------
ridgeRegression <- function(train, test){
  warn <- err <- NULL
  
  train_sparse <- sparse.model.matrix(~., train[, colnames(train) != 'y', with=FALSE]) # matrix formula (~.) and independent vars
  test_sparse <- sparse.model.matrix(~., test[, colnames(test) != 'y', with=FALSE])
  # remove intercept added by model.matrix
  train_sparse <- train_sparse[, colnames(train_sparse) != '(Intercept)']
  test_sparse <- test_sparse[, colnames(test_sparse) != '(Intercept)']
  
  ################################
  ## alpha = 0 for RIDGE ---------
  
  # use cross-validation to find the ideal lambda to use
  train.cv <- withCallingHandlers(cv.glmnet(y=train$y, x=train_sparse, family="binomial", nfolds = 10, type.measure="deviance", parallel = FALSE, alpha = 0, standardize = TRUE),
                                  error = function(e){
                                    err <<- conditionMessage(e)
                                    NULL
                                  },
                                  warning = function(w){
                                    warn <<- append(warn, conditionMessage(w))
                                    #return(res)
                                    invokeRestart("muffleWarning")
                                  }) 
  print(paste0("lambda selected: ",train.cv$lambda.1se))
  #use the lambda.lse to fit at 1 standard-error from least squared error
  #letting glmnet standardize because it transforms the coefficients back automatically
  train.model <- withCallingHandlers(glmnet(y=train$y, x=train_sparse, family="binomial", lambda = train.cv$lambda.1se, alpha = 0, standardize = TRUE),
                                     error = function(e){
                                       err <<- conditionMessage(e)
                                       NULL
                                     },
                                     warning = function(w){
                                       warn <<- append(warn, conditionMessage(w))
                                       #return(res)
                                       invokeRestart("muffleWarning")
                                     }) 
  tmp_coeffs <- coef(train.model)
  coefficients <- data.frame(variable = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], Estimate = tmp_coeffs@x)
  
  # split out the intercept
  intercept <- coefficients[coefficients$variable == "(Intercept)",]
  coefficients <- coefficients[coefficients$variable != "(Intercept)",]
  # order coefficients by beta estimate
  coefficients <- coefficients[order(-coefficients$Estimate),]
  # add the intercept back in to the top of coefficients
  coefficients <- rbind(intercept, coefficients)
  response <- list(#model =train.model,
                   aic = NULL,
                   formula = as.formula(paste("y ~",paste0(coefficients$variable,collapse=" + "))),
                   coefficients = coefficients,
                   train.predicted = as.vector(predict(train.model, newx = train_sparse, s=train.cv$lambda.1se, type="response")),
                   test.predicted = as.vector(predict(train.model, newx = test_sparse, s=train.cv$lambda.1se, type="response")),
                   warnings = unique(warn),
                   errors = unique(err)
  )
  
  return(response)
} # end of ridgeRegression


###############################################################
## Elastic Net Regression -----------------------
elasticNetRegression <- function(train, test){
  warn <- err <- NULL

  train_sparse <- sparse.model.matrix(~., train[, colnames(train) != 'y', with=FALSE]) # matrix formula (~.) and independent vars
  test_sparse <- sparse.model.matrix(~., test[, colnames(test) != 'y', with=FALSE])
  # remove intercept added by model.matrix
  train_sparse <- train_sparse[, colnames(train_sparse) != '(Intercept)']
  test_sparse <- test_sparse[, colnames(test_sparse) != '(Intercept)']
  
  ################################
  ## 0 < alpha < 1 for RIDGE ---------
  
  a <- seq(0.1,0.9,0.05)  #set sequence/steps for alpha search
  # use cross-validation to find the ideal lambda to use
  search <- withCallingHandlers(foreach(i = a, .combine = rbind, .packages="glmnet") %do% {
                                    tmp.cv <- cv.glmnet(y = train$y, x=train_sparse, family = "binomial", nfold = 10, type.measure = "deviance", parallel = FALSE, alpha = i, standardize=TRUE)
                                    data.frame(cvm = tmp.cv$cvm[tmp.cv$lambda == tmp.cv$lambda.1se], lambda.1se = tmp.cv$lambda.1se, alpha = i)
                                  },
                                error = function(e){
                                  err <<- conditionMessage(e)
                                  NULL
                                },
                                warning = function(w){
                                  warn <<- append(warn, conditionMessage(w))
                                  #return(res)
                                  invokeRestart("muffleWarning")
                                })
  # select cv (contains lambda) with the minimum lambda.1se
  model.cv <- search[search$cvm == min(search$cvm), ]
  print(paste0("alpha selected: ",model.cv$alpha))
  print(paste0("lambda selected: ",model.cv$lambda.1se))
  #use the lambda.lse to fit at 1 standard-error from least squared error
  #letting glmnet standardize because it transforms the coefficients back automatically
  train.model <- withCallingHandlers(glmnet(y=train$y, x=train_sparse, family="binomial", lambda = model.cv$lambda.1se, alpha = model.cv$alpha, standardize = TRUE),
                                     error = function(e){
                                       err <<- conditionMessage(e)
                                       NULL
                                     },
                                     warning = function(w){
                                       warn <<- append(warn, conditionMessage(w))
                                       #return(res)
                                       invokeRestart("muffleWarning")
                                     }) 
  tmp_coeffs <- coef(train.model)
  coefficients <- data.frame(variable = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], Estimate = tmp_coeffs@x)
  
  # split out the intercept
  intercept <- coefficients[coefficients$variable == "(Intercept)",]
  coefficients <- coefficients[coefficients$variable != "(Intercept)",]
  # order coefficients by beta estimate
  coefficients <- coefficients[order(-coefficients$Estimate),]
  # add the intercept back in to the top of coefficients
  coefficients <- rbind(intercept, coefficients)
  response <- list(#model =train.model,
                   aic = NULL,
                   formula = as.formula(paste("y ~",paste0(coefficients$variable,collapse=" + "))),
                   coefficients = coefficients,
                   train.predicted = as.vector(predict(train.model, newx = train_sparse, s=model.cv$lambda.1se, type="response")),
                   test.predicted = as.vector(predict(train.model, newx = test_sparse, s=model.cv$lambda.1se, type="response")),
                   warnings = unique(warn),
                   errors = unique(err)
  )
  
  return(response)
} # end of elasticNetRegression



###############################################################
## randomForestClassification -----------------------
randomForestClassification <- function(train, test, sampling_methods=c("none","under","both"), modelName){  #will choose the best model from requested sampling techniques
  warn <- err <- NULL

  # We are going to further split test into VALID and TEST
  library(splitstackshape)
  splitTest <- stratified(test, group=c("y"), size=0.5, bothSets=TRUE)
  test.valid <- splitTest$SAMP1
  test.test <- splitTest$SAMP2
  
  train$y <- as.factor(train$y) # classification requires dependent variable to be a factor
  test$y <- as.factor(test$y)
  test.valid$y <- as.factor(test.valid$y)
  test.test$y <- as.factor(test.test$y)
  
  #how many levels?
  #y.table <- table(train$y)/nrow(train)
  #factorRatio <- min(y.table)/max(y.table) #use for automatic sampling

  #Generate random string to ensure unique H2O object names
  rand_string <- stri_rand_strings(n=1, length=8)
  
    
  sampling <- data.frame(sampling=character(), source=character(), local=character(), h2o=character(), stringsAsFactors = FALSE)


    # we have multiple options for sampling, including over, under, both, ROSE
  if('none' %in% sampling_methods) {
    sampling <- rbind(sampling, data.frame(sampling="none", source="train", local="train.h2o", h2o=paste0("train_h2o_none_",rand_string), stringsAsFactors = FALSE))
    
  }  
  if('over' %in% sampling_methods){
    # OVER SAMPLING
    train.over <- ROSE::ovun.sample(y~., 
                                    data=train,
                                    method="over",  # options:  over, under, both
                                    p=0.5           # proportion of rare class
                                    )[['data']]     # only return the resulting data
    train.over$y <- as.factor(train.over$y)
    sampling <- rbind(sampling, data.frame(sampling="over", source="train.over", local="train.h2o.over", h2o=paste0("train_h2o_over_",rand_string), stringsAsFactors = FALSE))
  }
  if('under' %in% sampling_methods){
    # UNDER SAMPLING
    train.under <- ROSE::ovun.sample(y~., 
                                     data=train,
                                     method="under", # options:  over, under, both
                                     p=0.5           # proportion of rare class
                                     )[['data']]     # only return the resulting data
    train.under$y <- as.factor(train.under$y)
    sampling <- rbind(sampling, data.frame(sampling="under", source="train.under", local="train.h2o.under", h2o=paste0("train_h2o_under_",rand_string), stringsAsFactors = FALSE))
  }
  if('both' %in% sampling_methods){
    # UNDER AND OVER SAMPLING
    train.both <- ROSE::ovun.sample(y~., 
                                    data=train,
                                    method="both",  # options:  over, under, both
                                    p=0.5           # proportion of rare class
                                    )[['data']]     # only return the resulting data
    train.both$y <- as.factor(train.both$y)
    sampling <- rbind(sampling, data.frame(sampling="both", source="train.both", local="train.h2o.both", h2o=paste0("train_h2o_both_",rand_string), stringsAsFactors = FALSE))
  }
  if('rose' %in% sampling_methods){
    # ROSE SAMPLING
    train.rose <- ROSE::ROSE(y~.,
                             data=train,
                             p=0.5           # proportion of rare class
                             )[['data']]     # only return the resulting data
    train.rose$y <- as.factor(train.rose$y)
    sampling <- rbind(sampling, data.frame(sampling="rose", source="train.rose", local="train.h2o.rose", h2o=paste0("train_h2o_rose_",rand_string), stringsAsFactors = FALSE))
  }
  #initialize h2o
  do.call(h2o::h2o.init, H2O_OPTS) # calling with predefined h2o options
  for(i in 1:nrow(sampling)){
    x <- sampling[i,]
    message(paste0(x[['local']]," <- as.h2o(", x[['source']],", destination_frame = \"",x[['h2o']],"\")"))
    assign(x[['local']], as.h2o(get(x[['source']]), destination_frame=x[['h2o']]))
  }
  train.h2o <- as.h2o(train, destination_frame=paste0("train_",rand_string)) # used for predictions
  test.h2o <- as.h2o(test, destination_frame=paste0("test_",rand_string)) # used for predictions
  test.valid.h2o <- as.h2o(test.valid, destination_frame=paste0("test.valid_",rand_string))
  test.test.h2o <- as.h2o(test.test, destination_frame=paste0("test.test_",rand_string))
  
  # assign x and y by names
  y.dep <- "y" 
  x.indep <- setdiff(colnames(train.h2o), y.dep) # choose all variables that are not 'y'
  
  # Create data.frame to store model results
  rforests <- data.frame(modelName=character(), training_frame=character(), trainAUC=numeric(), validAUC=numeric(), testAUC=numeric(), stringsAsFactors=FALSE)
  
  for(i in 1:nrow(sampling)){
    thisSample <- sampling[i,]
    modelID <- paste0("rForest_sampling_",thisSample$sampling,"_",rand_string)
    message("Running Classification for: ",modelID)
    
    # Using h2o to handle random forest classification
    train.model <- withCallingHandlers(h2o::h2o.randomForest(y=y.dep, 
                                                             x=x.indep, 
                                                             model_id = modelID,
                                                             training_frame = h2o::h2o.getFrame(thisSample$h2o), 
                                                             validation_frame = test.valid.h2o,
                                                             nfolds = 10,
                                                             fold_assignment = "Stratified",
                                                             balance_classes = FALSE,  # handling with over/under sampling
                                                             ntrees = 100, # default is 50, but we can safely increase when using stopping_rounds
                                                             #stopping_rounds = 2,  # Stop fitting new trees when the 2-tree average is within 0.001 (default value for stopping_tolerance) of the prior two 2-tree averages
                                                             #stopping_metric = "AUC",
                                                             #stopping_tolerance = .0001,
                                                             #mtries = mtry, 
                                                             #max_depth = 4, # default is 20
                                                             seed = 1122
                                       ),
                                       error = function(e){
                                         err <<- conditionMessage(e)
                                         NULL
                                       },
                                       warning = function(w){
                                         warn <<- append(warn, conditionMessage(w))
                                         #return(res)
                                         invokeRestart("muffleWarning")
                                       }) 
    
    thisAUC <- h2o::h2o.auc(train.model, train=TRUE, valid=TRUE)
    testAUC <- h2o::h2o.auc(h2o::h2o.performance(train.model, newdata=test.test.h2o))
    
    rforests <- rbind(rforests, data.frame(modelName = modelID,
                                   training_frame = thisSample$h2o,
                                   trainAUC = thisAUC[['train']],
                                   validAUC = thisAUC[['valid']],
                                   testAUC = testAUC,
                                   stringsAsFactors = FALSE))
    
  } # end for(sampleMethod in sampling)
  
  best <- rforests[rforests$testAUC == max(rforests$testAUC),][1,]
  
  bestModel <- h2o::h2o.getModel(best$modelName)

  tmp_coeffs <- h2o::h2o.varimp(bestModel)  
  coefficients <- data.frame(variable = tmp_coeffs$variable, Importance = tmp_coeffs$scaled_importance, Percentage = tmp_coeffs$percentage)
  
  train.h2o.predicted <- h2o::h2o.predict(bestModel, newdata=train.h2o)
  
  train.predicted <- as.data.frame(train.h2o.predicted)[,3]
  test.predicted = as.data.frame(h2o::h2o.predict(bestModel, newdata=test.h2o))[,3]
  
  # save H2O model
  file_path <- paste0('./saved models/',modelName)
  model_path <- h2o::h2o.saveModel(object = bestModel, path=file_path, force=TRUE)
  
  # clean up H2O objects
  current <- h2o::h2o.ls()
  keysToRemove <- as.vector(grep(pattern=rand_string, x=current$key, value=TRUE))
  uniqueKeysToRemove <- unique(keysToRemove)
  if(length(uniqueKeysToRemove) > 0){
    h2o::h2o.rm(uniqueKeysToRemove)
  }
  
  response <- list(#model =NA,
                   modelPath = model_path,
                   aic = NULL,
                   formula = NULL,
                   coefficients = coefficients,
                   train.predicted = train.predicted,
                   test.predicted = test.predicted,
                   warnings = unique(warn),
                   errors = unique(err)
  )
  return(response)
} # end of randomForestClassification


###############################################################
## Gradient Boosted Classification -----------------------
gbmClassification <- function(train, test, sampling_methods=c("none","under","both"), modelName){  #will choose the best model from requested sampling techniques
  warn <- err <- NULL
  
  # We are going to further split test into VALID and TEST
  library(splitstackshape)
  splitTest <- stratified(test, group=c("y"), size=0.5, bothSets=TRUE)
  test.valid <- splitTest$SAMP1
  test.test <- splitTest$SAMP2
  
  train$y <- as.factor(train$y) # classification requires dependent variable to be a factor
  test$y <- as.factor(test$y)
  test.valid$y <- as.factor(test.valid$y)
  test.test$y <- as.factor(test.test$y)
  #how many levels?
  #y.table <- table(train$y)/nrow(train)
  #factorRatio <- min(y.table)/max(y.table) #use for automatic sampling
  
  #Generate random string to ensure unique H2O object names
  rand_string <- stri_rand_strings(n=1, length=8)
  
  
  sampling <- data.frame(sampling=character(), source=character(), local=character(), h2o=character(), stringsAsFactors = FALSE)
  
  
  # we have multiple options for sampling, including over, under, both, ROSE
  if('none' %in% sampling_methods) {
    sampling <- rbind(sampling, data.frame(sampling="none", source="train", local="train.h2o", h2o=paste0("train_h2o_none_",rand_string), stringsAsFactors = FALSE))
    
  }  
  if('over' %in% sampling_methods){
    # OVER SAMPLING
    train.over <- ROSE::ovun.sample(y~., 
                                    data=train,
                                    method="over",  # options:  over, under, both
                                    p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.over$y <- as.factor(train.over$y)
    sampling <- rbind(sampling, data.frame(sampling="over", source="train.over", local="train.h2o.over", h2o=paste0("train_h2o_over_",rand_string), stringsAsFactors = FALSE))
  }
  if('under' %in% sampling_methods){
    # UNDER SAMPLING
    train.under <- ROSE::ovun.sample(y~., 
                                     data=train,
                                     method="under", # options:  over, under, both
                                     p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.under$y <- as.factor(train.under$y)
    sampling <- rbind(sampling, data.frame(sampling="under", source="train.under", local="train.h2o.under", h2o=paste0("train_h2o_under_",rand_string), stringsAsFactors = FALSE))
  }
  if('both' %in% sampling_methods){
    # UNDER AND OVER SAMPLING
    train.both <- ROSE::ovun.sample(y~., 
                                    data=train,
                                    method="both",  # options:  over, under, both
                                    p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.both$y <- as.factor(train.both$y)
    sampling <- rbind(sampling, data.frame(sampling="both", source="train.both", local="train.h2o.both", h2o=paste0("train_h2o_both_",rand_string), stringsAsFactors = FALSE))
  }
  if('rose' %in% sampling_methods){
    # ROSE SAMPLING
    train.rose <- ROSE::ROSE(y~.,
                             data=train,
                             p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.rose$y <- as.factor(train.rose$y)
    sampling <- rbind(sampling, data.frame(sampling="rose", source="train.rose", local="train.h2o.rose", h2o=paste0("train_h2o_rose_",rand_string), stringsAsFactors = FALSE))
  }
  #initialize h2o
  do.call(h2o::h2o.init, H2O_OPTS) # calling with predefined h2o options
  for(i in 1:nrow(sampling)){
    x <- sampling[i,]
    message(paste0(x[['local']]," <- as.h2o(", x[['source']],", destination_frame = \"",x[['h2o']],"\")"))
    assign(x[['local']], as.h2o(get(x[['source']]), destination_frame=x[['h2o']]))
  }
  train.h2o <- as.h2o(train, destination_frame=paste0("train_",rand_string)) # used for predictions
  test.h2o <- as.h2o(test, destination_frame=paste0("test_",rand_string)) # used for predictions
  test.valid.h2o <- as.h2o(test.valid, destination_frame=paste0("test.valid_",rand_string))
  test.test.h2o <- as.h2o(test.test, destination_frame=paste0("test.test_",rand_string))
  # assign x and y by names
  y.dep <- "y" 
  x.indep <- setdiff(colnames(train.h2o), y.dep) # choose all variables that are not 'y'
  
  # Create data.frame to store model results
  gbms <- data.frame(modelName=character(), training_frame=character(), trainAUC=numeric(), validAUC=numeric(), testAUC=numeric(), stringsAsFactors=FALSE)
  
  for(i in 1:nrow(sampling)){
    thisSample <- sampling[i,]
    modelID <- paste0("gbm_sampling_",thisSample$sampling,"_",rand_string)
    message("Running Classification for: ",modelID)
    

    
    
    # Using h2o to handle GBM classification
    train.model <- withCallingHandlers(h2o::h2o.gbm(y=y.dep, 
                                                    x=x.indep, 
                                                    model_id = modelID,
                                                    training_frame = h2o::h2o.getFrame(thisSample$h2o), 
                                                    validation_frame = test.valid.h2o,
                                                    nfolds = 10,
                                                    fold_assignment = "Stratified",
                                                    balance_classes = FALSE,  # handling with over/under sampling
                                                    learn_rate = 0.01,
                                                    seed = 1122
    ),
    error = function(e){
      err <<- conditionMessage(e)
      NULL
    },
    warning = function(w){
      warn <<- append(warn, conditionMessage(w))
      #return(res)
      invokeRestart("muffleWarning")
    }) 
    thisAUC <- h2o::h2o.auc(train.model, train=TRUE, valid=TRUE)
    testAUC <- h2o::h2o.auc(h2o::h2o.performance(train.model, newdata=test.test.h2o))

    gbms <- rbind(gbms, data.frame(modelName = modelID,
                                   training_frame = thisSample$h2o,
                                   trainAUC = thisAUC[['train']],
                                   validAUC = thisAUC[['valid']],
                                   testAUC = testAUC,
                                   stringsAsFactors = FALSE))
    
  } # end for(sampleMethod in sampling)
  
  best <- gbms[gbms$testAUC == max(gbms$testAUC),][1,]
  
  bestModel <- h2o::h2o.getModel(best$modelName)
  
  tmp_coeffs <- h2o::h2o.varimp(bestModel)  
  coefficients <- data.frame(variable = tmp_coeffs$variable, Importance = tmp_coeffs$scaled_importance, Percentage = tmp_coeffs$percentage)
  
  train.h2o.predicted <- h2o::h2o.predict(bestModel, newdata=train.h2o)
  
  train.predicted <- as.data.frame(train.h2o.predicted)[,3]
  test.predicted = as.data.frame(h2o::h2o.predict(bestModel, newdata=test.h2o))[,3]
  
  # save H2O model
  file_path <- paste0('./saved models/',modelName)
  model_path <- h2o::h2o.saveModel(object = bestModel, path=file_path, force=TRUE)
  
  # clean up H2O objects
  current <- h2o::h2o.ls()
  keysToRemove <- as.vector(grep(pattern=rand_string, x=current$key, value=TRUE))
  uniqueKeysToRemove <- unique(keysToRemove)
  if(length(uniqueKeysToRemove) > 0){
    h2o::h2o.rm(uniqueKeysToRemove)
  }
  
  response <- list(#model =NA,
                   modelPath = model_path,
                   aic = NULL,
                   formula = NULL,
                   coefficients = coefficients,
                   train.predicted = train.predicted,
                   test.predicted = test.predicted,
                   warnings = unique(warn),
                   errors = unique(err)
  )
  return(response)
} # end of gbmClassification


###############################################################
## Extended Gradient Boosted Classification -----------------------
extendedGBMClassification <- function(train, test, sampling_methods=c("none","under","both"), modelName){  #will choose the best model from requested sampling techniques
  warn <- err <- NULL
  
  # We are going to further split test into VALID and TEST
  library(splitstackshape)
  splitTest <- stratified(test, group=c("y"), size=0.5, bothSets=TRUE)
  test.valid <- splitTest$SAMP1
  test.test <- splitTest$SAMP2
  
  train$y <- as.factor(train$y) # classification requires dependent variable to be a factor
  test$y <- as.factor(test$y)
  test.valid$y <- as.factor(test.valid$y)
  test.test$y <- as.factor(test.test$y)
  #how many levels?
  #y.table <- table(train$y)/nrow(train)
  #factorRatio <- min(y.table)/max(y.table) #use for automatic sampling
  
  #Generate random string to ensure unique H2O object names
  rand_string <- stri_rand_strings(n=1, length=8)
  
  
  sampling <- data.frame(sampling=character(), source=character(), local=character(), h2o=character(), stringsAsFactors = FALSE)
  
  
  # we have multiple options for sampling, including over, under, both, ROSE
  if('none' %in% sampling_methods) {
    sampling <- rbind(sampling, data.frame(sampling="none", source="train", local="train.h2o", h2o=paste0("train_h2o_none_",rand_string), stringsAsFactors = FALSE))
    
  }  
  if('over' %in% sampling_methods){
    # OVER SAMPLING
    train.over <- ROSE::ovun.sample(y~., 
                                    data=train,
                                    method="over",  # options:  over, under, both
                                    p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.over$y <- as.factor(train.over$y)
    sampling <- rbind(sampling, data.frame(sampling="over", source="train.over", local="train.h2o.over", h2o=paste0("train_h2o_over_",rand_string), stringsAsFactors = FALSE))
  }
  if('under' %in% sampling_methods){
    # UNDER SAMPLING
    train.under <- ROSE::ovun.sample(y~., 
                                     data=train,
                                     method="under", # options:  over, under, both
                                     p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.under$y <- as.factor(train.under$y)
    sampling <- rbind(sampling, data.frame(sampling="under", source="train.under", local="train.h2o.under", h2o=paste0("train_h2o_under_",rand_string), stringsAsFactors = FALSE))
  }
  if('both' %in% sampling_methods){
    # UNDER AND OVER SAMPLING
    train.both <- ROSE::ovun.sample(y~., 
                                    data=train,
                                    method="both",  # options:  over, under, both
                                    p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.both$y <- as.factor(train.both$y)
    sampling <- rbind(sampling, data.frame(sampling="both", source="train.both", local="train.h2o.both", h2o=paste0("train_h2o_both_",rand_string), stringsAsFactors = FALSE))
  }
  if('rose' %in% sampling_methods){
    # ROSE SAMPLING
    train.rose <- ROSE::ROSE(y~.,
                             data=train,
                             p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.rose$y <- as.factor(train.rose$y)
    sampling <- rbind(sampling, data.frame(sampling="rose", source="train.rose", local="train.h2o.rose", h2o=paste0("train_h2o_rose_",rand_string), stringsAsFactors = FALSE))
  }
  #initialize h2o
  do.call(h2o::h2o.init, H2O_OPTS) # calling with predefined h2o options
  for(i in 1:nrow(sampling)){
    x <- sampling[i,]
    message(paste0(x[['local']]," <- as.h2o(", x[['source']],", destination_frame = \"",x[['h2o']],"\")"))
    assign(x[['local']], as.h2o(get(x[['source']]), destination_frame=x[['h2o']]))
  }
  train.h2o <- as.h2o(train, destination_frame=paste0("train_",rand_string)) # used for predictions
  test.h2o <- as.h2o(test, destination_frame=paste0("test_",rand_string)) # used for predictions
  test.valid.h2o <- as.h2o(test.valid, destination_frame=paste0("test.valid_",rand_string))
  test.test.h2o <- as.h2o(test.test, destination_frame=paste0("test.test_",rand_string))
  # assign x and y by names
  y.dep <- "y" 
  x.indep <- setdiff(colnames(train.h2o), y.dep) # choose all variables that are not 'y'
  
  # Create data.frame to store model results
  gbms <- data.frame(modelName=character(), training_frame=character(), trainAUC=numeric(), validAUC=numeric(), stringsAsFactors=FALSE)
  
#  for(i in 1:nrow(sampling)){
    thisSample <- sampling[i,]
    modelID <- paste0("gbm_sampling_",thisSample$sampling,"_",rand_string)
    message("Running Classification for: ",modelID)
    
        
    ########################
    # https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/product/tutorials/gbm/gbmTuning.Rmd
    gbm <- h2o::h2o.gbm(x=x.indep, y=y.dep, training_frame = train.h2o)
    h2o::h2o.auc(h2o::h2o.performance(gbm, newdata=test.valid.h2o))
    
    gbm2 <- h2o::h2o.gbm(x=x.indep, y=y.dep, training_frame = h2o::h2o.rbind(train.h2o, test.valid.h2o), nfolds = 4, seed=0xDECAF)
    gbm2@model$cross_validation_metrics_summary
    h2o::h2o.auc(h2o::h2o.performance(gbm2, xval=TRUE))
    
    # search for best max_depth
    hyper_params <- list(max_depth = c(4,6,8,12,16,20,25,29))
    grid <- h2o::h2o.grid(
      hyper_params = hyper_params,
      search_criteria = list(strategy = "Cartesian"),
      algorithm = "gbm",
      grid_id = "depth_grid",
      x=x.indep,
      y=y.dep,
      training_frame = train.h2o,
      validation_frame = test.valid.h2o,
      ntrees = 1000,
      learn_rate = 0.05,
      learn_rate_annealing = 0.99,
      sample_rate = 0.8,
      seed=1234,
      stopping_rounds=5, stopping_tolerance = 0.0001, stopping_metric = "AUC",
      score_tree_interval = 10
      
    )
    grid
    sortedGrid <- h2o::h2o.getGrid("depth_grid", sort_by="auc", decreasing=TRUE)
    sortedGrid
    topDepths = sortedGrid@summary_table$max_depth[1:5]
    minDepth = min(as.numeric(topDepths))
    maxDepth = max(as.numeric(topDepths))
    minDepth
    maxDepth
    
    # search for other hyperparameter values
    hyper_params = list(
      max_depth = seq(minDepth,maxDepth,1),
      sample_rate = seq(0.2,1,0.01),
      col_sample_rate = seq(0.2,1,0.01),
      col_sample_rate_per_tree = seq(0.2,1,0.01),
      col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
      min_rows = 2^seq(0,log2(nrow(train.h2o))-1,1),
      nbins = 2^seq(4,10,1),
      nbins_cats = 2^seq(4,10,1), 
      min_split_improvement = c(0,1e-8,1e-6,1e-4), 
      histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
    )
    search_criteria = list(
      strategy = "RandomDiscrete",      
      max_runtime_secs = 3600,         
      max_models = 100,                  
      seed = 1234,                        
      stopping_rounds = 5,                
      stopping_metric = "AUC",
      stopping_tolerance = 1e-3
    )
    grid <- h2o::h2o.grid(
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      algorithm = "gbm",
      grid_id = "final_grid", 
      x = x.indep, 
      y = y.dep, 
      training_frame = train.h2o, 
      validation_frame = test.valid.h2o,
      ntrees = 10000,                                                            
      learn_rate = 0.05,                                                         
      learn_rate_annealing = 0.99,                                               
      max_runtime_secs = 3600,                                                 
      stopping_rounds = 5, stopping_tolerance = 1e-3, stopping_metric = "AUC", 
      score_tree_interval = 10,                                                
      seed = 1234                                                             
    )
    sortedGrid <- h2o::h2o.getGrid("final_grid", sort_by="auc", decreasing=TRUE)
    sortedGrid
    
    bestModel <- h2o::h2o.getModel(sortedGrid@model_ids[[i]])
#    for(i in 1:5){
#      gbm3 <- h2o::h2o.getModel(sortedGrid@model_ids[[i]])
#      print(h2o::h2o.auc(h2o::h2o.performance(gbm3, valid=TRUE)))
#      message(h2o::h2o.auc(gbm3, valid=TRUE))
#    }
#    gbm4 <- h2o::h2o.getModel(sortedGrid@model_ids[[1]])
#    print(h2o::h2o.auc(h2o::h2o.performance(gbm4, newdata=test.test.h2o)))
#    gbm4@parameters
#    plot(gbm4, metric="auc")
    ##########################

    
    bestModel <- gbm4
    
    tmp_coeffs <- h2o::h2o.varimp(bestModel)  
    coefficients <- data.frame(variable = tmp_coeffs$variable, Importance = tmp_coeffs$scaled_importance, Percentage = tmp_coeffs$percentage)
    
    train.h2o.predicted <- h2o::h2o.predict(bestModel, newdata=train.h2o)
    
    train.predicted <- as.data.frame(train.h2o.predicted)[,3]
    test.predicted = as.data.frame(h2o::h2o.predict(bestModel, newdata=test.h2o))[,3]
    
    # save H2O model
    file_path <- paste0('./saved models/',modelName)
    model_path <- h2o::h2o.saveModel(object = bestModel, path=file_path, force=TRUE)
    response <- list(#model =NA,
                     modelPath = model_path,
                     aic = NULL,
                     formula = NULL,
                     coefficients = coefficients,
                     train.predicted = train.predicted,
                     test.predicted = test.predicted,
                     warnings = unique(warn),
                     errors = unique(err)
    )
    return(response)
} # end of extendedGBMClassification    
    
    
    
    
###############################################################
## XG Boost Classification -----------------------
xgboostClassification <- function(train, test, sampling_methods=c("none","under","both"), modelName){  #will choose the best model from requested sampling techniques
  warn <- err <- NULL
  
  
  # We are going to further split test into VALID and TEST
  library(splitstackshape)
  splitTest <- stratified(test, group=c("y"), size=0.5, bothSets=TRUE)
  test.valid <- splitTest$SAMP1
  test.test <- splitTest$SAMP2
  
  train$y <- as.factor(train$y) # classification requires dependent variable to be a factor
  test$y <- as.factor(test$y)
  test.valid$y <- as.factor(test.valid$y)
  test.test$y <- as.factor(test.test$y)
  
  
  #how many levels?
  #y.table <- table(train$y)/nrow(train)
  #factorRatio <- min(y.table)/max(y.table) #use for automatic sampling
  
  #Generate random string to ensure unique H2O object names
  rand_string <- stri_rand_strings(n=1, length=8)
  
  
  sampling <- data.frame(sampling=character(), source=character(), local=character(), h2o=character(), stringsAsFactors = FALSE)
  
  
  # we have multiple options for sampling, including over, under, both, ROSE
  if('none' %in% sampling_methods) {
    sampling <- rbind(sampling, data.frame(sampling="none", source="train", local="train.h2o", h2o=paste0("train_h2o_none_",rand_string), stringsAsFactors = FALSE))
    
  }  
  if('over' %in% sampling_methods){
    # OVER SAMPLING
    train.over <- ROSE::ovun.sample(y~., 
                                    data=train,
                                    method="over",  # options:  over, under, both
                                    p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.over$y <- as.factor(train.over$y)
    sampling <- rbind(sampling, data.frame(sampling="over", source="train.over", local="train.h2o.over", h2o=paste0("train_h2o_over_",rand_string), stringsAsFactors = FALSE))
  }
  if('under' %in% sampling_methods){
    # UNDER SAMPLING
    train.under <- ROSE::ovun.sample(y~., 
                                     data=train,
                                     method="under", # options:  over, under, both
                                     p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.under$y <- as.factor(train.under$y)
    sampling <- rbind(sampling, data.frame(sampling="under", source="train.under", local="train.h2o.under", h2o=paste0("train_h2o_under_",rand_string), stringsAsFactors = FALSE))
  }
  if('both' %in% sampling_methods){
    # UNDER AND OVER SAMPLING
    train.both <- ROSE::ovun.sample(y~., 
                                    data=train,
                                    method="both",  # options:  over, under, both
                                    p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.both$y <- as.factor(train.both$y)
    sampling <- rbind(sampling, data.frame(sampling="both", source="train.both", local="train.h2o.both", h2o=paste0("train_h2o_both_",rand_string), stringsAsFactors = FALSE))
  }
  if('rose' %in% sampling_methods){
    # ROSE SAMPLING
    train.rose <- ROSE::ROSE(y~.,
                             data=train,
                             p=0.5           # proportion of rare class
    )[['data']]     # only return the resulting data
    train.rose$y <- as.factor(train.rose$y)
    sampling <- rbind(sampling, data.frame(sampling="rose", source="train.rose", local="train.h2o.rose", h2o=paste0("train_h2o_rose_",rand_string), stringsAsFactors = FALSE))
  }
  #initialize h2o
  do.call(h2o::h2o.init, H2O_OPTS) # calling with predefined h2o options
  for(i in 1:nrow(sampling)){
    x <- sampling[i,]
    message(paste0(x[['local']]," <- as.h2o(", x[['source']],", destination_frame = \"",x[['h2o']],"\")"))
    assign(x[['local']], as.h2o(get(x[['source']]), destination_frame=x[['h2o']]))
  }
  train.h2o <- as.h2o(train, destination_frame=paste0("train_",rand_string)) # used for predictions
  test.h2o <- as.h2o(test, destination_frame=paste0("test_",rand_string)) # used for predictions
  test.valid.h2o <- as.h2o(test.valid, destination_frame=paste0("test.valid_",rand_string))
  test.test.h2o <- as.h2o(test.test, destination_frame=paste0("test.test_",rand_string))

  # assign x and y by names
  y.dep <- "y" 
  x.indep <- setdiff(colnames(train.h2o), y.dep) # choose all variables that are not 'y'
  
  # Create data.frame to store model results
  xgboosts <- data.frame(modelName=character(), training_frame=character(), trainAUC=numeric(), validAUC=numeric(), testAUC=numeric(), stringsAsFactors=FALSE)
  
  for(i in 1:nrow(sampling)){
    thisSample <- sampling[i,]
    modelID <- paste0("xgboost_sampling_",thisSample$sampling,"_",rand_string)
    message("Running Classification for: ",modelID)
    
    # Using h2o to handle random forest classification
    train.model <- withCallingHandlers(h2o::h2o.xgboost(y=y.dep, 
                                                       x=x.indep, 
                                                       model_id = modelID,
                                                       training_frame = h2o::h2o.getFrame(thisSample$h2o), 
                                                       validation_frame = test.valid.h2o,
                                                       nfolds = 10,
                                                       fold_assignment = "Stratified",
                                                       learn_rate = 0.01,
                                                       seed = 1122
                                                       ),
                  error = function(e){
                    err <<- conditionMessage(e)
                    NULL
                  },
                  warning = function(w){
                    warn <<- append(warn, conditionMessage(w))
                    #return(res)
                    invokeRestart("muffleWarning")
                  }) 
    
    thisAUC <- h2o::h2o.auc(train.model, train=TRUE, valid=TRUE)
    testAUC <- h2o::h2o.auc(h2o::h2o.performance(train.model, newdata=test.test.h2o))
    
    xgboosts <- rbind(xgboosts, data.frame(modelName = modelID,
                                           training_frame = thisSample$h2o,
                                           trainAUC = thisAUC[['train']],
                                           validAUC = thisAUC[['valid']],
                                           testAUC = testAUC,
                                           stringsAsFactors = FALSE))

  } # end for(sampleMethod in sampling)
  
  best <- xgboosts[xgboosts$testAUC == max(xgboosts$testAUC),][1,]
  
  bestModel <- h2o::h2o.getModel(best$modelName)
  
  tmp_coeffs <- h2o::h2o.varimp(bestModel)  
  coefficients <- data.frame(variable = tmp_coeffs$variable, Importance = tmp_coeffs$scaled_importance, Percentage = tmp_coeffs$percentage)
  
  train.h2o.predicted <- h2o::h2o.predict(bestModel, newdata=train.h2o)
  
  train.predicted <- as.data.frame(train.h2o.predicted)[,3]
  test.predicted = as.data.frame(h2o::h2o.predict(bestModel, newdata=test.h2o))[,3]
  
  # save H2O model
  file_path <- paste0('./saved models/',modelName)
  model_path <- h2o::h2o.saveModel(object = bestModel, path=file_path, force=TRUE)
  
  # clean up H2O objects
  current <- h2o::h2o.ls()
  keysToRemove <- as.vector(grep(pattern=rand_string, x=current$key, value=TRUE))
  uniqueKeysToRemove <- unique(keysToRemove)
  if(length(uniqueKeysToRemove) > 0){
    h2o::h2o.rm(uniqueKeysToRemove)
  }
  
  response <- list(#model =NA,
                   modelPath = model_path,
                   aic = NULL,
                   formula = NULL,
                   coefficients = coefficients,
                   train.predicted = train.predicted,
                   test.predicted = test.predicted,
                   warnings = unique(warn),
                   errors = unique(err)
  )
  return(response)
} # end of xgboostClassification
