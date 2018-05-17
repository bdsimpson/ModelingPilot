# logistic test run

# install.packages("RODBC")
library("RODBC")
library("brglm")
library("tibble")

#getwd()
getwd()
#setwd()
setwd("C:/Users/bsimp/Documents/P&G")

#paste examples
tmp1 <- "Hello World"
tmp2 <- paste("Hello","World")
tmp3 <- paste0("Hello","World")
message(tmp1)
message(tmp2)
message(tmp3)

# With Separator
tmp2 <- paste("Hello","World", sep=" + ")
tmp3 <- paste0("Hello ","World")
message(tmp2)
message(tmp3)



## Connect to 1,CP (RODBC)
tconn <- odbcConnect(dsn="1CP-64")

sql <- "SELECT * FROM TWM_SANDBOX.MO_FRA_2018_OralBManual_2_MU"

modeldata <- sqlQuery(tconn, sql)
odbcClose(tconn) #close the connection


#to output the dataset
write.csv(modeldata, file = "modeldata.csv")


#see what columns are in the dataset
colnames(modeldata)


#get rid of some variables
modeldata$MATCHD_CNSMR_PRSNA_ID <- NULL
modeldata$D_HR_BRD_OralB <- NULL
modeldata$N_TENUREMONTHS <- NULL
modeldata$NUM_KIDS <- NULL
modeldata$C_TA_BRD_OralB <- NULL
modeldata$C_TA_BRD_OralBManual <- NULL

colnames(modeldata)

#rename target variable
colnames(modeldata)[1] <- 'y'

colnames(modeldata)


#Full Logistic Regression
# formula options:
#    y ~ .   -- y = everything
#    y ~ CNT_IV + N_AGE + .....  -- specified variables
model.brglm <- brglm(y ~ ., family=binomial, data=modeldata)

#overall model results
summary(model.brglm)


################
## Look at some of the components

# Get the result coefficients and put them in a data frame
coeff <- data.frame(Estimate = coef(model.brglm)) #can view this with: View(coeff)

# convert the row names into a column
coeff2 <- data.frame(Variable = rownames(coeff), Estimate = coeff$Estimate)

# simplified output
as.tibble(coeff2)

message(paste0("Model AIC: ", model.brglm$aic))


