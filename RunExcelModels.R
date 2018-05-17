#################################################################
#  Run Excel Models.R
#
#  This script looks for an inbox that will hold excel documents
#     with a formatted config tab.  
#  Overall Process:
#  1) Search inbox for input files
#  2) Read in values from config file
#  3) Query Teradata database for the designated Source Table
#     - Teradata connection made via RODBC
#     - odbcConnect requires a named DSN
#       - If Running R in 32-bit mode, this DSN must be set up as 32-bit
#       - If Running R in 64-bit mode, this DSN must be set up as 64-bit
#       - Can use saved username/password from DSN or as specified by:
#           odbcConnect(dsn, uid=<username>, pwd=<password>)
#  4) Modify and/or Filter the data based on config parameters
#  5) Split dataset into training and test datasets
#  6) Load previous model results if they exist
#  7) Run required regression models.  
#       Options:
#         - Full Logistic Regression
#         - Stepwise Logistic Regression
#         - LASSO Regression
#         - RIDGE Regression
#         - Elastic Net Regression
#       Each option will run the regression and output results
#         as tabs in the input excel file
#  8) Save model results
#  9) Move excel file to the outbox
# 10) Repeat steps 2-9 for each excel file in the inbox (excluding template.xlsx)
#
#################################################################

# set this to the location of the 'ModelingPilot' folder
setwd("~/projects/ModelingPilot")
inbox <- './workbooks - Inbox'
outbox <- './workbooks - Outbox'



#######################################################
## Search inbox for excel files -----------------------
excelFiles <- list.files(path=inbox, pattern='^[[:alnum:]].*[.]xlsx$', full.names=TRUE)
#ignore config template
excelFiles <- excelFiles[basename(excelFiles) != "template.xlsx"]

#######################################################
## Set some necessary options
options(java.parameters = "-Xmx4g") # set java stack size to 4G (default 512MB)

## Import required libraries/scripts ------------------
source('./libraries/utilities.R')   # general utilities
# Include external libraries
loadOrInstall(c(
  "RJDBC",   # needed for JDBC connection to Teradata
  "dplyr",   # ranking tools, some cool filter/dataframe functions
  "splitstackshape",   # using to split train/test with stratefied sampling
  "jsonlite" # library used to parse JSON data
  )
)

# load local libraries
source('./libraries/regression.R')  # Regression function wrappers
source('./libraries/diagnostics.R') # regression diagnostics
source('./libraries/xlsx-helper.R') # wrapper and helpers for xlsx

credentials <- fromJSON("~/credentials.json")

#######################################################
## Establish JDBC driver
# set this to the location of the teradata JDBC .jar files
JAR_DIR <- path.expand("~/drivers/teradataJDBC/")
.jinit()
.jaddClassPath(paste(JAR_DIR, "terajdbc4.jar", sep=""))
.jaddClassPath(paste(JAR_DIR, "tdgssconfig.jar", sep=""))
drv <- JDBC(driverClass="com.teradata.jdbc.TeraDriver")


##############################################################
## Process each excel file in the inbox ----------------------
for(excelFile in excelFiles){
  
  ## load excel file and pull in config information ------------
  wb <- newWorkbook(file=excelFile)
  config <- read.xlsx(excelFile, sheetName = "Config", header=FALSE)
  
  # read in control information from excel sheet
  modelName <- as.character(filter(config, X1 == "Model Name")$X2)
  sourceTable <- as.character(filter(config, X1 == "Source Table")$X2)
  targetVariable <- as.character(filter(config, X1 == "Target Variable")$X2)
  includeExclude <- as.character(filter(config, X1 == "Include / Exclude")$X2)
  scoringTable <- as.character(filter(config, X1 == "Scoring Table")$X2)
  scoringIndexColumns <- as.character(filter(config, X1 == "Scoring Index Columns")$X2)
  scoringCriteria <- as.character(filter(config, X1 == "Scoring Criteria")$X2)
  scoreVariable <- as.character(filter(config, X1 == "Score Variable")$X2)

  
  columns.col.index <- as.numeric(which(apply(config, 2, function(x) any(grepl("Columns (Include/Exclude)", x, fixed = TRUE))))) # find the column that contains the EXACT text
  columns <- as.character(config[!is.na(config[,columns.col.index]),columns.col.index])[-1]  

  models.col.index <- as.numeric(which(apply(config, 2, function(x) any(grepl("Models to Run", x, fixed = TRUE))))) # find the column that contains the EXACT text
  models <- as.character(config[!is.na(config[,models.col.index + 1]) & toupper(config[,models.col.index + 1]) == "TRUE",models.col.index])

  #set some default behavior in case no values are passed in for scoring
  if (is.na(scoringTable) || length(scoringTable) == 0) scoringTable <- "<scoring table placeholder>"
  if (is.na(scoringIndexColumns) || length(scoringIndexColumns) == 0) scoringIndexColumns = ""
  if (is.na(scoringCriteria) || length(scoringCriteria) == 0) scoringCriteria <- ""
  if (is.na(scoreVariable) || length(scoreVariable) == 0) scoreVariable <- paste0(gsub("[[:space:]]","_",modelName),"_Score")
  
  #######################################################
  ## Query Teradata for designated Source Table----------
  
  livedata <- TRUE
  
  if(livedata){
    message("Fetching data from 1,CP")
    ## Connect to 1,CP (JDBC)
    tconn <- dbConnect(drv, credentials$teradataJDBC$connString,credentials$teradataJDBC$username,credentials$teradataJDBC$password)
    
    modeldata <- dbGetQuery(tconn, paste0("SELECT * FROM ",sourceTable))
    dbDisconnect(tconn) #close the connection
  }else{
    message("Loading data from local file")
    load(file=paste0('./data/',modelName,".Rda")) # imports previously saved 'modeldata' object
  }
  # optionally save the dataset for offline use (only use for testing)
  save(modeldata, file=paste0('./data/',modelName,".Rda"))
  
  
  ###################
  #### base cleanup
  # rename target variable to 'y'
  names(modeldata)[names(modeldata) == targetVariable] <- 'y'
  
  if(includeExclude == 'Exclude'){
    # remove variables flagged for exclusion
    modeldata <- select(modeldata, -one_of(columns))
  }else if (includeExclude == 'Include'){
    # only keep variables flagged for inclusion
    modeldata <- select(modeldata, one_of(columns))
  }
  # Remove any variables that are constant
  for(var in names(modeldata)[names(modeldata) != 'y']){
    var.levels <- length(unique(modeldata[,var]))
    if(var.levels == 1){
      modeldata <- select(modeldata, -one_of(c(var)))
      message(paste0("WARNING: Removing Constant Variable: ",var))
    }
  }
  
  
  # split into train/test
  set.seed(1234)
  
  testStrat <- stratified(modeldata, group=c("y"), size=0.6, bothSets=TRUE)
  
  train <- testStrat$SAMP1
  test <- testStrat$SAMP2

  
  # Keep running object will complete results
  resultsFile <- paste0('./saved models/',modelName,".Rda") 
  if(!file.exists(resultsFile)){
    RESULTS <- list()
  }else{
    load(file=resultsFile)
  }
  # Temporarily keep results from THIS RUN Only
  THISRUN <- list()
  
  ###########################################
  ## Logistic Regression --------------------
  if("Full Logistic" %in% models){
    # run full logistic regression
    message("Running Full Logistic Regression...")
    sheet.runtime <- system.time({
      runtime <- system.time(
        result <- fullLogistic(train = train, test = test)
      )
      messages <- c(result$errors,result$warnings)
      # output results to Excel worksheet
      sheet <- newSheet(wb, sheetName="Full Logistic", sheetTitle="Full Logistic Regression", overwrite=FALSE, runtime=runtime[['elapsed']], messages=c(result$errors,result$warnings))
      RESULTS[[sheet$getSheetName()]] <- result
      THISRUN[[sheet$getSheetName()]] <- result
      groupBottom <- 2 + length(messages)
  
      # AUC
      groupTop <- groupBottom + 2
      auc <- addAUC(wb,sheet, row=groupTop, column=1, train.predicted = result$train.predicted, train.actual = train$y, test.predicted = result$test.predicted, test.actual = test$y)
      groupBottom <- auc$lastRow
      #Lift Tables/Charts
      groupTop <- groupBottom + 3
      trainlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Train: Full Logistic Regression", predicted=result$train.predicted, actual=train$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- trainlift$lastRow
      groupTop <- groupBottom + 3
      testlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Test: Full Logistic Regression", predicted=result$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- testlift$lastRow
  
      #Coefficients
      groupTop <- groupBottom + 5
      coef <- addCoefficients(wb,sheet, row=groupTop, column=1, df=result$coefficients)
      
      #Correlation Diagnostics/Correlation Matrix
      corrDiagnostics <- addCorrelationDiagnostics(wb, sheet, row=groupTop, column=8, df=train, frm=result$formula)
      correlations <- addCorrelations(wb,sheet, row=corrDiagnostics$lastRow + 2, column = 8, df=train, frm=result$formula)
      groupBottom <- max(coef$lastRow, correlations$lastRow)
  
      #Confusion Matrix
      groupTop <- groupBottom + 4
      confMatrixTrain <- addConfusionMatrix(wb,sheet,row=groupTop, column=1, actual=train$y, predicted=result$train.predicted, title="Confusion Matrix (cutoff at top 20%)", subtitle="Train:")
      confMatrixTest <- addConfusionMatrix(wb, sheet, row = groupTop, column=8, actual=test$y, predicted=result$test.predicted, title="", subtitle="Test:" )
      groupBottom <- max(confMatrixTrain$lastRow, confMatrixTest$lastRow)
      
      #Scoring SQL
      groupTop <- groupBottom + 4    
      scoreSql <- addScoringSql(wb, sheet, row=groupTop, column=1, df=result$coefficients)
      groupBottom <- scoreSql$lastRow
    })
    if(length(sheet.runtime) > 0){
      runtime.hours <- floor(sheet.runtime[['elapsed']]/3600)
      runtime.minutes <- floor((sheet.runtime[['elapsed']] - 3600*runtime.hours)/60)
      runtime.seconds <- floor(sheet.runtime[['elapsed']] - 3600*runtime.hours - 60*runtime.minutes)
      addText(sheet, rowIndex = groupBottom + 2, text=paste0("Full Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
      saveWorkbook(wb, file=excelFile)
    }
    gc()
  }
  ###################################################################
  
  #########################################
  ## Stepwise regression using bigstep ----
  if("Forward Logistic" %in% models){
    sheet.runtime <- system.time({
      runtime <- system.time(
        result <- forwardLogistic(train = train, test = test)
      )
      messages <- c(result$errors,result$warnings)
      # output results to Excel worksheet
      sheet <- newSheet(wb, sheetName="Forward Logistic", sheetTitle="Forward Stepwise Logistic Regression", overwrite=FALSE, runtime=runtime[['elapsed']], messages=c(result$errors,result$warnings))
      RESULTS[[sheet$getSheetName()]] <- result
      THISRUN[[sheet$getSheetName()]] <- result
      groupBottom <- 2 + length(messages)
      
      #AUC
      groupTop <- groupBottom + 2
      auc <- addAUC(wb,sheet, row=groupTop, column=1, train.predicted = result$train.predicted, train.actual = train$y, test.predicted = result$test.predicted, test.actual = test$y)
      groupBottom <- auc$lastRow
      
      # Lift Tables/Charts
      groupTop <- groupBottom + 3
      trainlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Train: Forward Logistic Regression", predicted=result$train.predicted, actual=train$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- trainlift$lastRow
      groupTop <- groupBottom + 3
      testlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Test: Forward Logistic Regression", predicted=result$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- testlift$lastRow
      
      # Coefficients
      groupTop <- groupBottom + 5
      coef <- addCoefficients(wb,sheet, row=groupTop, column=1, df=result$coefficients)
      # Correlation Diagnostics/Correlation Matrix
      corrDiagnostics <- addCorrelationDiagnostics(wb, sheet, row=groupTop, column=8, df=train, frm=result$formula)
      correlations <- addCorrelations(wb,sheet, row=corrDiagnostics$lastRow + 2, column = 8, df=train, frm=result$formula)
      groupBottom <- max(coef$lastRow, correlations$lastRow)
      
      # Confusion Matrix
      groupTop <- groupBottom + 4
      confMatrixTrain <- addConfusionMatrix(wb,sheet,row=groupTop, column=1, actual=train$y, predicted=result$train.predicted, title="Confusion Matrix (cutoff at top 20%)", subtitle="Train:")
      confMatrixTest <- addConfusionMatrix(wb, sheet, row = groupTop, column=8, actual=test$y, predicted=result$test.predicted, title="", subtitle="Test:" )
      groupBottom <- max(confMatrixTrain$lastRow, confMatrixTest$lastRow)
      
      groupTop <- groupBottom + 4
      scoreSql <- addScoringSql(wb, sheet, row=groupTop, column=1, df=result$coefficients)
      groupBottom <- scoreSql$lastRow
    })
    if(length(sheet.runtime) > 0){
      runtime.hours <- floor(sheet.runtime[['elapsed']]/3600)
      runtime.minutes <- floor((sheet.runtime[['elapsed']] - 3600*runtime.hours)/60)
      runtime.seconds <- floor(sheet.runtime[['elapsed']] - 3600*runtime.hours - 60*runtime.minutes)
      addText(sheet, rowIndex = groupBottom + 2, text=paste0("Full Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
      saveWorkbook(wb, file=excelFile)
    }
    gc()
  }
  ###################################################################
  
  #########################################
  ## LASSO Regression ---------------------
  if("LASSO" %in% models){
    sheet.runtime <- system.time({
      runtime <- system.time(
        result <- lassoRegression(train = train, test = test)
      )
      messages <- c(result$errors,result$warnings)
      # output results to Excel worksheet
      sheet <- newSheet(wb, sheetName="LASSO", sheetTitle="LASSO Regression", overwrite=FALSE, runtime=runtime[['elapsed']], messages=c(result$errors,result$warnings))
      RESULTS[[sheet$getSheetName()]] <- result
      THISRUN[[sheet$getSheetName()]] <- result
      groupBottom <- 2 + length(messages)
      
      # AUC
      groupTop <- groupBottom + 2
      auc <- addAUC(wb,sheet, row=groupTop, column=1, train.predicted = result$train.predicted, train.actual = train$y, test.predicted = result$test.predicted, test.actual = test$y)
      groupBottom <- auc$lastRow
      
      # Lift Tables/Charts
      groupTop <- groupBottom + 3
      trainlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Train: LASSO Regression", predicted=result$train.predicted, actual=train$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- trainlift$lastRow
      groupTop <- groupBottom + 3
      testlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Test: LASSO Regression", predicted=result$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- testlift$lastRow
      
      # Coefficients
      groupTop <- groupBottom + 5
      coef <- addCoefficients(wb,sheet, row=groupTop, column=1, df=result$coefficients)
      groupBottom <- coef$lastRow
      
      # Confusion Matrix
      groupTop <- groupBottom + 4
      confMatrixTrain <- addConfusionMatrix(wb,sheet,row=groupTop, column=1, actual=train$y, predicted=result$train.predicted, title="Confusion Matrix (cutoff at top 20%)", subtitle="Train:")
      confMatrixTest <- addConfusionMatrix(wb, sheet, row = groupTop, column=8, actual=test$y, predicted=result$test.predicted, title="", subtitle="Test:" )
      groupBottom <- max(confMatrixTrain$lastRow, confMatrixTest$lastRow)
      
      
      # Scoring SQL
      groupTop <- groupBottom + 3
      scoreSql <- addScoringSql(wb, sheet, row=groupTop, column=1, df=result$coefficients)
      groupBottom <- scoreSql$lastRow
    })
    if(length(sheet.runtime) > 0){
      runtime.hours <- floor(sheet.runtime[['elapsed']]/3600)
      runtime.minutes <- floor((sheet.runtime[['elapsed']] - 3600*runtime.hours)/60)
      runtime.seconds <- floor(sheet.runtime[['elapsed']] - 3600*runtime.hours - 60*runtime.minutes)
      addText(sheet, rowIndex = groupBottom + 2, text=paste0("Full Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
      saveWorkbook(wb, file=excelFile)
    }
    gc()
  }
  ###################################################################
  
  
  #########################################
  ## RIDGE Regression ---------------------
  if("RIDGE" %in% models) {
    sheet.runtime <- system.time({
      runtime <- system.time(
        result <- ridgeRegression(train = train, test = test)
      )
      messages <- c(result$errors,result$warnings)
      # output results to Excel worksheet
      sheet <- newSheet(wb, sheetName="RIDGE", sheetTitle="RIDGE Regression", overwrite=FALSE, runtime=runtime[['elapsed']], messages=c(result$errors,result$warnings))
      RESULTS[[sheet$getSheetName()]] <- result
      THISRUN[[sheet$getSheetName()]] <- result
      groupBottom <- 2 + length(messages)
      
      # AUC
      groupTop <- groupBottom + 2
      auc <- addAUC(wb,sheet, row=groupTop, column=1, train.predicted = result$train.predicted, train.actual = train$y, test.predicted = result$test.predicted, test.actual = test$y)
      groupBottom <- auc$lastRow
      
      #Lift Tables/Charts
      groupTop <- groupBottom + 3
      trainlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Train: RIDGE Regression", predicted=result$train.predicted, actual=train$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- trainlift$lastRow
      groupTop <- groupBottom + 3
      testlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Test: RIDGE Regression", predicted=result$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- testlift$lastRow
      
      # Coefficients
      groupTop <- groupBottom + 5
      coef <- addCoefficients(wb,sheet, row=groupTop, column=1, df=result$coefficients)
      groupBottom <- coef$lastRow
      
      # Confusion Matrix
      groupTop <- groupBottom + 4
      confMatrixTrain <- addConfusionMatrix(wb,sheet,row=groupTop, column=1, actual=train$y, predicted=result$train.predicted, title="Confusion Matrix (cutoff at top 20%)", subtitle="Train:")
      confMatrixTest <- addConfusionMatrix(wb, sheet, row = groupTop, column=8, actual=test$y, predicted=result$test.predicted, title="", subtitle="Test:" )
      groupBottom <- max(confMatrixTrain$lastRow, confMatrixTest$lastRow)
      
      # Scoring SQL
      groupTop <- groupBottom + 3
      scoreSql <- addScoringSql(wb, sheet, row=groupTop, column=1, df=result$coefficients)
      groupBottom <- scoreSql$lastRow
    })
    if(length(sheet.runtime) > 0){
      runtime.hours <- floor(sheet.runtime[['elapsed']]/3600)
      runtime.minutes <- floor((sheet.runtime[['elapsed']] - 3600*runtime.hours)/60)
      runtime.seconds <- floor(sheet.runtime[['elapsed']] - 3600*runtime.hours - 60*runtime.minutes)
      addText(sheet, rowIndex = groupBottom + 2, text=paste0("Full Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
      saveWorkbook(wb, file=excelFile)
    }
    gc()
  }
  ###################################################################
  
  
  #########################################
  ## Elastic Net Regression ---------------------
  if("Elastic Net" %in% models){
    sheet.runtime <- system.time({
      runtime <- system.time(
        result <- elasticNetRegression(train = train, test = test)
      )
      messages <- c(result$errors,result$warnings)
      # output results to Excel worksheet
      sheet <- newSheet(wb, sheetName="Elastic Net", sheetTitle="Elastic Net Regression", overwrite=FALSE, runtime=runtime[['elapsed']], messages=c(result$errors,result$warnings))
      RESULTS[[sheet$getSheetName()]] <- result
      THISRUN[[sheet$getSheetName()]] <- result
      groupBottom <- 2 + length(messages)
      
      # AUC
      groupTop <- groupBottom + 2
      auc <- addAUC(wb,sheet, row=groupTop, column=1, train.predicted = result$train.predicted, train.actual = train$y, test.predicted = result$test.predicted, test.actual = test$y)
      groupBottom <- auc$lastRow
      
      # Lift Tables/Charts
      groupTop <- groupBottom + 3
      trainlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Train: Elastic Net Regression", predicted=result$train.predicted, actual=train$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- trainlift$lastRow
      groupTop <- groupBottom + 3
      testlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Test: Elastic Net Regression", predicted=result$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- testlift$lastRow
      
      # Coefficients
      groupTop <- groupBottom + 5
      coef <- addCoefficients(wb,sheet, row=groupTop, column=1, df=result$coefficients)
      groupBottom <- coef$lastRow
      
      # Confusion Matrix
      groupTop <- groupBottom + 4
      confMatrixTrain <- addConfusionMatrix(wb,sheet,row=groupTop, column=1, actual=train$y, predicted=result$train.predicted, title="Confusion Matrix (cutoff at top 20%)", subtitle="Train:")
      confMatrixTest <- addConfusionMatrix(wb, sheet, row = groupTop, column=8, actual=test$y, predicted=result$test.predicted, title="", subtitle="Test:" )
      groupBottom <- max(confMatrixTrain$lastRow, confMatrixTest$lastRow)
      
      # Scoring SQL
      groupTop <- groupBottom + 3
      scoreSql <- addScoringSql(wb, sheet, row=groupTop, column=1, df=result$coefficients)
      groupBottom <- scoreSql$lastRow
    })
    if(length(sheet.runtime) > 0){
      runtime.hours <- floor(sheet.runtime[['elapsed']]/3600)
      runtime.minutes <- floor((sheet.runtime[['elapsed']] - 3600*runtime.hours)/60)
      runtime.seconds <- floor(sheet.runtime[['elapsed']] - 3600*runtime.hours - 60*runtime.minutes)
      addText(sheet, rowIndex = groupBottom + 2, text=paste0("Full Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
      saveWorkbook(wb, file=excelFile)
    }
    gc()
  }
  ###################################################################
  
  #########################################
  ## Random Forest Regression ---------------------
  if("Random Forest" %in% models){
    sheet.runtime <- system.time({
      runtime <- system.time(
        result <- randomForestClassification(train = train, test = test, modelName=modelName, sampling_methods=c("under","both","none"))
      )
      messages <- c(result$errors,result$warnings)
      # output results to Excel worksheet
      sheet <- newSheet(wb, sheetName="Random Forest", sheetTitle="Random Forest Regression", overwrite=FALSE, runtime=runtime[['elapsed']], messages=c(result$errors,result$warnings))
      RESULTS[[sheet$getSheetName()]] <- result
      THISRUN[[sheet$getSheetName()]] <- result
      groupBottom <- 2 + length(messages)
      
      # AUC
      groupTop <- groupBottom + 2
      auc <- addAUC(wb,sheet, row=groupTop, column=1, train.predicted = result$train.predicted, train.actual = train$y, test.predicted = result$test.predicted, test.actual = test$y)
      groupBottom <- auc$lastRow
      
      # Lift Tables/Charts
      groupTop <- groupBottom + 3
      trainlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Train: Random Forest Regression", predicted=result$train.predicted, actual=train$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- trainlift$lastRow
      groupTop <- groupBottom + 3
      testlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Test: Random Forest Regression", predicted=result$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- testlift$lastRow
      
      # Coefficients
      groupTop <- groupBottom + 5
      coef <- addCoefficients(wb,sheet, row=groupTop, column=1, df=result$coefficients)
      impPlot <- addImportancePlot(wb, sheet, row=groupTop, column=5, df=result$coefficients)
      groupBottom <- coef$lastRow
      
      # Confusion Matrix
      groupTop <- groupBottom + 4
      confMatrixTrain <- addConfusionMatrix(wb,sheet,row=groupTop, column=1, actual=train$y, predicted=result$train.predicted, title="Confusion Matrix (cutoff at top 20%)", subtitle="Train:")
      confMatrixTest <- addConfusionMatrix(wb, sheet, row = groupTop, column=8, actual=test$y, predicted=result$test.predicted, title="", subtitle="Test:" )
      groupBottom <- max(confMatrixTrain$lastRow, confMatrixTest$lastRow)
      
      # no sql generation for Random Forest.  Will have to develop alternative plan
      #groupTop <- groupBottom + 3
      #scoreSql <- addScoringSql(wb, sheet, row=groupTop, column=1, df=result$coefficients)
      #groupBottom <- scoreSql$lastRow
    })
    if(length(sheet.runtime) > 0){
      runtime.hours <- floor(sheet.runtime[['elapsed']]/3600)
      runtime.minutes <- floor((sheet.runtime[['elapsed']] - 3600*runtime.hours)/60)
      runtime.seconds <- floor(sheet.runtime[['elapsed']] - 3600*runtime.hours - 60*runtime.minutes)
      addText(sheet, rowIndex = groupBottom + 2, text=paste0("Full Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
      saveWorkbook(wb, file=excelFile)
    }
    gc()
  }
  ###################################################################
  
  #########################################
  ## Gradient Boost Classification ---------------------
  if("GBM" %in% models){
    sheet.runtime <- system.time({
      runtime <- system.time(
        result <- gbmClassification(train = train, test = test, modelName=modelName, sampling_methods=c("under","both","none"))
      )
      messages <- c(result$errors,result$warnings)
      # output results to Excel worksheet
      sheet <- newSheet(wb, sheetName="GBM", sheetTitle="GBM Classification", overwrite=FALSE, runtime=runtime[['elapsed']], messages=c(result$errors,result$warnings))
      RESULTS[[sheet$getSheetName()]] <- result
      THISRUN[[sheet$getSheetName()]] <- result
      groupBottom <- 2 + length(messages)
      
      # AUC
      groupTop <- groupBottom + 2
      auc <- addAUC(wb,sheet, row=groupTop, column=1, train.predicted = result$train.predicted, train.actual = train$y, test.predicted = result$test.predicted, test.actual = test$y)
      groupBottom <- auc$lastRow
      
      # Lift Tables/Charts
      groupTop <- groupBottom + 3
      trainlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Train: GBM Classification", predicted=result$train.predicted, actual=train$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- trainlift$lastRow
      groupTop <- groupBottom + 3
      testlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Test: GBM Classification", predicted=result$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- testlift$lastRow
      
      # Coefficients
      groupTop <- groupBottom + 5
      coef <- addCoefficients(wb,sheet, row=groupTop, column=1, df=result$coefficients)
      impPlot <- addImportancePlot(wb, sheet, row=groupTop, column=5, df=result$coefficients)
      groupBottom <- coef$lastRow
      
      # Confusion Matrix
      groupTop <- groupBottom + 4
      confMatrixTrain <- addConfusionMatrix(wb,sheet,row=groupTop, column=1, actual=train$y, predicted=result$train.predicted, title="Confusion Matrix (cutoff at top 20%)", subtitle="Train:")
      confMatrixTest <- addConfusionMatrix(wb, sheet, row = groupTop, column=8, actual=test$y, predicted=result$test.predicted, title="", subtitle="Test:" )
      groupBottom <- max(confMatrixTrain$lastRow, confMatrixTest$lastRow)
      
      # no sql generation for Random Forest.  Will have to develop alternative plan
      #groupTop <- groupBottom + 3
      #scoreSql <- addScoringSql(wb, sheet, row=groupTop, column=1, df=result$coefficients)
      #groupBottom <- scoreSql$lastRow
    })
    if(length(sheet.runtime) > 0){
      runtime.hours <- floor(sheet.runtime[['elapsed']]/3600)
      runtime.minutes <- floor((sheet.runtime[['elapsed']] - 3600*runtime.hours)/60)
      runtime.seconds <- floor(sheet.runtime[['elapsed']] - 3600*runtime.hours - 60*runtime.minutes)
      addText(sheet, rowIndex = groupBottom + 2, text=paste0("Full Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
      saveWorkbook(wb, file=excelFile)
    }
    gc()
  }
  ###################################################################

  #########################################
  ## XGBoost Classification ---------------------
  if("XGBoost" %in% models){
    sheet.runtime <- system.time({
      runtime <- system.time(
        result <- xgboostClassification(train = train, test = test, modelName=modelName, sampling_methods=c("under","both","none"))
      )
      messages <- c(result$errors,result$warnings)
      # output results to Excel worksheet
      sheet <- newSheet(wb, sheetName="XGBoost", sheetTitle="XGBoost Classification", overwrite=FALSE, runtime=runtime[['elapsed']], messages=c(result$errors,result$warnings))
      RESULTS[[sheet$getSheetName()]] <- result
      THISRUN[[sheet$getSheetName()]] <- result
      groupBottom <- 2 + length(messages)
      
      # AUC
      groupTop <- groupBottom + 2
      auc <- addAUC(wb,sheet, row=groupTop, column=1, train.predicted = result$train.predicted, train.actual = train$y, test.predicted = result$test.predicted, test.actual = test$y)
      groupBottom <- auc$lastRow
      
      # Lift Tables/Charts
      groupTop <- groupBottom + 3
      trainlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Train: XGBoost Classification", predicted=result$train.predicted, actual=train$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- trainlift$lastRow
      groupTop <- groupBottom + 3
      testlift <- addLiftTables(wb, sheet, row=groupTop,column=1, chartTitle="Test: XGBoost Classification", predicted=result$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      groupBottom <- testlift$lastRow
      
      # Coefficients
      groupTop <- groupBottom + 5
      coef <- addCoefficients(wb,sheet, row=groupTop, column=1, df=result$coefficients)
      impPlot <- addImportancePlot(wb, sheet, row=groupTop, column=5, df=result$coefficients)
      groupBottom <- coef$lastRow
      
      # Confusion Matrix
      groupTop <- groupBottom + 4
      confMatrixTrain <- addConfusionMatrix(wb,sheet,row=groupTop, column=1, actual=train$y, predicted=result$train.predicted, title="Confusion Matrix (cutoff at top 20%)", subtitle="Train:")
      confMatrixTest <- addConfusionMatrix(wb, sheet, row = groupTop, column=8, actual=test$y, predicted=result$test.predicted, title="", subtitle="Test:" )
      groupBottom <- max(confMatrixTrain$lastRow, confMatrixTest$lastRow)
      
      # no sql generation for Random Forest.  Will have to develop alternative plan
      #groupTop <- groupBottom + 3
      #scoreSql <- addScoringSql(wb, sheet, row=groupTop, column=1, df=result$coefficients)
      #groupBottom <- scoreSql$lastRow
    })
    if(length(sheet.runtime) > 0){
      runtime.hours <- floor(sheet.runtime[['elapsed']]/3600)
      runtime.minutes <- floor((sheet.runtime[['elapsed']] - 3600*runtime.hours)/60)
      runtime.seconds <- floor(sheet.runtime[['elapsed']] - 3600*runtime.hours - 60*runtime.minutes)
      addText(sheet, rowIndex = groupBottom + 2, text=paste0("Full Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
      saveWorkbook(wb, file=excelFile)
    }
    gc()
  }
  ###################################################################
  
  ## Create/Edit Summary Page
  sumSheet <- newSheet(wb, sheetName="Summary", sheetTitle="Model Summaries", overwrite=FALSE, timestamp=Sys.time())
  sheetRow <- 3
  if(length(THISRUN) > 0){
    for(i in 1:length(THISRUN)){
      modelRow <- sheetRow
      model <- THISRUN[i]
      modelName <- names(model)
      #message(modelName)

      addText(sumSheet, rowIndex = modelRow, text=modelName, textStyle=getStyle(wb, "SUBTITLE"))
      modelRow <- modelRow + 2
      auc <- addAUC(wb,sumSheet, row=modelRow, column=2, train.predicted = model[[1]]$train.predicted, train.actual = train$y, test.predicted = model[[1]]$test.predicted, test.actual = test$y)
      modelRow <- auc$lastRow + 2
      testlift <- addLiftTables(wb, sumSheet, row=modelRow,column=2, chartTitle=paste0("Lift Metrics: ",modelName), predicted=model[[1]]$test.predicted, actual=test$y, cumLift=TRUE, pctResponse=TRUE)
      modelRow <- testlift$lastRow + 4
      addText(sumSheet, rowIndex = modelRow, colIndex = 2, text="5 Most Influential Variables:", textStyle=getStyle(wb,"BOLD"))
      modelRow <- modelRow + 2
      coef <- model[[1]]$coefficients
      # get rid of Intercept
      coef <- coef[coef$variable != '(Intercept)',]
      if('Estimate' %in% names(coef)){
        coef <- coef[order(-abs(coef$Estimate)),]
      }
      if('Importance' %in% names(coef)){
        coef <- coef[order(-coef$Importance),]
      }
      coef <- coef[1:5,]
      xlsx::addDataFrame(coef, sumSheet, startRow = modelRow, startColumn = 2, col.names=TRUE, row.names=FALSE)
      #xlsx::autoSizeColumn(sheet, colIndex=column:(column+1))
      xlsx::setColumnWidth(sumSheet,colIndex=2, colWidth=50 )
      modelRow <- modelRow + nrow(coef) + 2
      
      sheetRow <- modelRow + 2
    }
  }else{
    addText(sumSheet, rowIndex = sheetRow, text="No Models to Evaluate", textStyle=getStyle(wb, "ERROR"))
  }
  saveWorkbook(wb, file=excelFile)
    
  ##################################
  ## Cleanup / Finish Loop ---------
  
  
  # save completed models
  save(RESULTS, file=resultsFile)
  
  # move file to outbox
  if (!isTRUE(file.info(outbox)$isdir)) dir.create(outbox, recursive=TRUE)
  file.rename(from = excelFile, to=paste0(outbox,'/',basename(excelFile)))

} #end of for excelFile in excelFiles

