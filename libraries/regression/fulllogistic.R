



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
  response <- list(model = train.model,
                   aic = train.model$aic,
                   formula = paste("Formula: y ~",paste0(names(train.model$coefficients),collapse=" + ")),
                   coefficients = coefficients,
                   train.predicted = predict(train.model, type="response"),
                   test.predicted = predict(train.model, newdata = test, type="response"),
                   warnings = unique(warn),
                   errors = unique(err)
  )
  return(response)
} # end of fullLogistic()

