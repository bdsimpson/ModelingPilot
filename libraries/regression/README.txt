################################################################
  regression.R

  This script brings in required libraries and defines functions
     for regression models
  - Ultimately these will be hidden in a package

  Includes:
   1) fullLogistic()
   2) forwardLogistic()
   3) forwardLogisticOld() -- keeping for reference
   4) lassoRegression()
   5) ridgeRegression()
   6) elasticNetRegression()
   7) randomForest()

  Each Regression function expects the following inputs:
    train   - data frame containing training data
    test    - data frame containing testing data

   * these data frames should have the following characteristics:
     - Dependent variable named 'y'
     - No missing values

  Each Regression function should return a list object containing the following:
    model           - full regression results
    aic             - Calculated Akaike Information Criterion (NULL for glmnet models)
    formula         - Text representation of final formula 
    coefficients    - data frame containing selected variables and their coefficients
    train.predicted - vector of y-hat values for the training dataset
    test.predicted  - vector of y-hat values for the testing dataset
    warnings        - collection of any warnings encountered during regression
    errors          - collection of any errors encountered during regression

################################################################