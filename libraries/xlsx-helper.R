#################################################################
#  xlsx-helper.R
#
#  This script brings in required libraries and defines functions
#     for writing model results to an excel file
#  - Ultimately these will be hidden in a package
#
#  Wrappers for 'xlsx' built-in functions created for easier use:
#   1) addText()
#   2) newWorkbook()
#   3) getStyle()
#   4) newSheet()
#   5) getCells()
#   6) styleCells()
#   7) addCellComment()
#
#  Work-horse functions for outputting styled model results
#   1) addCoefficients()
#   2) addCorrelationDiagnostics()
#   3) addCorrelations()
#   4) addAUC()
#   5) addLiftTables()
#   6) addScoringSql()
#   7) addImportancePlot()
#
#  Each Work-horse function should 
#    a) Insert the required information into the worksheet
#    b) Return a list object with the following (used for templating):
#      firstRow 
#      firstColumn 
#      lastRow 
#      lastColumn 
#
#################################################################


## Load required libraries if not loaded
if(!require(xlsx)){
  install.packages("xlsx")
  library(xlsx)
}
if(!require(mltools)){
  install.packages("mltools")
  library(mltools)
}

#### addText: Helper function to add titles
# - sheet : sheet object to contain the title
# - rowIndex : numeric value indicating the row to contain the text
# - colIndex : numeric value indicating the column to contain the text (default=1)
# - text : the text to insert into cell
# - textStyle : style object to use for cell text (default is workbook default)
addText<-function(sheet, rowIndex, colIndex=1, text, textStyle=CellStyle(wb)){
  rows <- xlsx::getRows(sheet, rowIndex = rowIndex)
  if(length(rows) == 0){
    rows <- xlsx::createRow(sheet,rowIndex=rowIndex)
  }
  cells <- xlsx::createCell(rows, colIndex)
  targetCell <- cells[[1,1]] # pull cell out of list of cells
  xlsx::setCellValue(targetCell, text)
  xlsx::setCellStyle(targetCell, textStyle)
} # end of addText



#wrapper for create/load Workbook in xlsx
newWorkbook <- function(file=NULL){
  if(is.null(file)){
    wb <- xlsx::createWorkbook()
  }else{
    wb <- xlsx::loadWorkbook(file=file)
  }
  return(wb)
}

## Define workbook styles ------------------------
getStyle <- function(wb, styleName){
  style <- switch(styleName,
   "NORMAL" = xlsx::CellStyle(wb),
         # Title Styles
         "TITLE" = xlsx::CellStyle(wb)+ xlsx::Font(wb, heightInPoints=16, color="blue", isBold=TRUE),
         "SUBTITLE" = xlsx::CellStyle(wb) + xlsx::Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=FALSE),
         "TABLE_ROWNAMES" = xlsx::CellStyle(wb) + xlsx::Font(wb, isBold=TRUE),
         "TABLE_COLNAMES" = xlsx::CellStyle(wb) + xlsx::Font(wb, isBold=TRUE) +
           xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
           xlsx::Border(color="black", position=c("TOP", "BOTTOM"), 
                        pen=c("BORDER_THIN", "BORDER_THICK")),
         "ERROR" = xlsx::CellStyle(wb) + xlsx::Font(wb, color="red", isBold=TRUE),
         # Correlation Matrix coloring
         "CORR_BAD" = xlsx::CellStyle(wb) + xlsx::Fill(foregroundColor = "#FF0000"), #red
         "CORR_DANGER" = xlsx::CellStyle(wb) + xlsx::Fill(foregroundColor = "#FF8888"), #pinkish
         # Indented
         "INDENT_1" = xlsx::CellStyle(wb) + xlsx::Alignment(horizontal="ALIGN_LEFT", indent = 4),
         "INDENT_2" = xlsx::CellStyle(wb) + xlsx::Alignment(horizontal="ALIGN_LEFT", indent = 8),
         # Borders
         "GRID" = xlsx::CellStyle(wb) + 
            xlsx::Border(color="black",position=c("TOP","LEFT","RIGHT","BOTTOM"),pen="BORDER_THIN") + 
            xlsx::CellStyle(wb) + DataFormat("#,##0"),
         # General
         "BOLD" = xlsx::CellStyle(wb) + xlsx::Font(wb, isBold=TRUE)
         )
  return(style)
}

## create new worksheet in existing workbook
# delete old sheet if one exists
newSheet <- function(wb, sheetName, sheetTitle=sheetName, overwrite = TRUE, runtime=NULL, messages=NULL, timestamp=NULL){
  this.sheet <- NULL
  sheetNames <- names(getSheets(wb))

  if(sheetName %in% sheetNames){ # tab already exists
    if(overwrite){
      removeSheet(wb, sheetName = sheetName) # remove existing worksheet
    }else{
      tryThis <- sheetName
      i <- 2
      while(tryThis %in% sheetNames){
        tryThis <- paste(sheetName,i)
        i <- i + 1
      }
      sheetName <- tryThis
    }
  }
  this.sheet <- createSheet(wb, sheetName = sheetName)
  rowIndex <- 1
  addText(this.sheet, rowIndex, text=sheetTitle, textStyle=getStyle(wb, "TITLE"))
  rowIndex <- rowIndex + 1
  if(length(timestamp) > 0){
    addText(this.sheet, rowIndex, text=paste0("Created at: ",format(timestamp, tz="America/Chicago",usetz=TRUE)))
    rowIndex <- rowIndex + 1
  }
  if(length(runtime) > 0){
    runtime.hours <- floor(runtime/3600)
    runtime.minutes <- floor((runtime - 3600*runtime.hours)/60)
    runtime.seconds <- floor(runtime - 3600*runtime.hours - 60*runtime.minutes)
    addText(this.sheet, rowIndex, text=paste0("Model Execution Time: ",runtime.hours," hours, ",runtime.minutes," minutes, ",runtime.seconds," seconds"))
    rowIndex <- rowIndex + 1
  }
  if(length(messages) > 0){
    for(i in 1:length(messages)){
      addText(this.sheet, rowIndex = rowIndex - 1 + i, text=messages[i], textStyle=getStyle(wb, "ERROR"))
    }
  }
  xlsx::setColumnWidth(this.sheet, colIndex=c(1:7), colWidth=10)
  xlsx::setColumnWidth(this.sheet, colIndex=c(8:20), colWidth=20)
  saveWorkbook(wb, file=excelFile)
  return(this.sheet)
} # end of newSheet()

## Return a collection of cells based on specified rows,columns
# rows/columns can be a single value or a range (ie rows=5 or rows=1:10)
getCells <- function(sheet, rows, columns){
  this.rows <- xlsx::getRows(sheet=sheet, rowIndex = rows)
  this.cells <- xlsx::getCells(row = this.rows, colIndex = columns)
  return(this.cells)
}

## Apply a defined style object to a collection of cells
styleCells <- function(cells, cellStyle){
  lapply(cells, function(cell){
    xlsx::setCellStyle(cell, cellStyle=cellStyle)
  })
}

addCellComment <- function(sheet, rowIndex, colIndex, text, visible=TRUE){
  cell <- getCells(sheet=sheet, rows = rowIndex, columns=colIndex)[[1]]
  xlsx::createCellComment(cell, string=text, visible=visible)
}


#####################################################################
## Add table of selected variables and coefficients -----------------
addCoefficients <- function(wb, sheet, row, column, df){
  numVars <- sapply(df,is.numeric)
  df[numVars] <- lapply(df[numVars], round, digits=5)
  addText(sheet, rowIndex=row, colIndex = column, text="Variable Coefficients", textStyle=getStyle(wb, "SUBTITLE"))
  xlsx::addDataFrame(df, sheet, startRow = row + 2, startColumn = column, col.names=TRUE, row.names=FALSE)
  #xlsx::autoSizeColumn(sheet, colIndex=column:(column+1))
  xlsx::setColumnWidth(sheet,colIndex=column, colWidth=50 )
  lastRow <- row + 2 + nrow(df)
  lastColumn <- column + ncol(df)
  # set column header style
  headerCells <- getCells(sheet=sheet, rows= row + 2, columns = column:lastColumn)
  dummy <- styleCells(headerCells, cellStyle = getStyle(wb, "TABLE_COLNAMES"))
  # set row header style
  headerCells <- getCells(sheet=sheet, rows=(row + 3):lastRow, columns=column)
  dummy <- styleCells(headerCells, cellStyle = getStyle(wb, "TABLE_ROWNAMES"))
  saveWorkbook(wb, file=excelFile)
  return(list(
    firstRow = row,
    firstColumn = column,
    lastRow = lastRow,
    lastColumn = lastColumn
  ))
}


#####################################################################
## Add VIF and CI correlation diagnostics  --------------------------
addCorrelationDiagnostics <- function(wb, sheet, row, column, df, frm, vifThreshold=4, ciThreshold=30, problemsOnly=TRUE){
  addText(sheet, rowIndex=row, colIndex = column, text="Correlation Diagnostics", textStyle=getStyle(wb, "SUBTITLE"))
  addText(sheet, rowIndex=row+2, colIndex=column, text="Variance Inflation Factor (VIF)")
  row <- row + 3

  # Variance Inflaction Factor
  depvars <- labels(terms(frm))
  depvars <- depvars[depvars != 'Intercept']
  df.x <- data.frame(select(df,depvars))
  vif <- generateVIF(df.x, threshold=vifThreshold)
  vifTable <- NULL
  if(problemsOnly){
    vifTable <- round(data.frame(t(vif$problem), check.names=FALSE),4)
  }else{
    vifTable <- round(data.frame(t(vif$full), check.names=FALSE),4)
  }
  if(length(vifTable) > 0){
    addDataFrame(vifTable, sheet, startRow = row, startColumn=column, col.names=TRUE, row.names=FALSE)
    lastRow <- row + nrow(vifTable)
    lastColumn <- column + ncol(vifTable)
    # Column Names
    headerCells <- getCells(sheet=sheet, rows=row, columns=column:lastColumn)
    dummy <- styleCells(headerCells, cellStyle=getStyle(wb, "TABLE_COLNAMES"))
    xlsx::setColumnWidth(sheet, colIndex=c(column:lastColumn), colWidth=20)
  }else{
    addText(sheet, rowIndex=row+1, colIndex=column, text="No issues detected.")
    lastRow <- row + 1
    lastColumn <- column + 3
  }
#####  Disabling Condition Indices until I can find a compatible package with the AWS server
#  #Condition Index
#  row <- lastRow
#  addText(sheet, rowIndex=row+2, colIndex=column, text="Condition Index (CI)")
#  row <- row + 3
#  ci <- generateCI(model, threshold=ciThreshold)
#  ciTable <- NULL
#  if(problemsOnly){
#    ciTable <- round(data.frame(ci$problem, check.names=FALSE),4)
#  }else{
#    ciTable <- round(data.frame(ci$full, check.names=FALSE),4)
#  }
#  if(length(ciTable) > 0){
#    addDataFrame(ciTable, sheet, startRow = row, startColumn=column, col.names=TRUE, row.names=FALSE)
#    lastRow <- row + nrow(ciTable)
#    lastColumn <- column + ncol(ciTable)
#    # Column Names
#    headerCells <- getCells(sheet=sheet, rows=row, columns=column:lastColumn)
#    dummy <- styleCells(headerCells, cellStyle=getStyle(wb, "TABLE_COLNAMES"))
#    autoSizeColumn(sheet, colIndex=c(column:lastColumn))
#  }else{
#    addText(sheet, rowIndex=row, colIndex=column, text="No issues detected.")
#    lastRow <- row + 1
#    lastColumn <- column + 3
#  }
  saveWorkbook(wb, file=excelFile)
  return(list(
    firstRow = row,
    firstColumn = column,
    lastRow = lastRow,
    lastColumn = lastColumn
  ))
} # end of addCorrelationDiagnostics()


#####################################################################
## Add Formatted Correlation Matrix  --------------------------------
addCorrelations <- function(wb, sheet, row, column, df, frm){
  addText(sheet, rowIndex=row, colIndex = column, text="Correlation Matrix", textStyle=getStyle(wb, "SUBTITLE"))
  depvars <- labels(terms(frm))
  depvars <- depvars[depvars != 'Intercept']
  cormatrix <- generateReducedCorrelationMatrix(df, depvars)
  addDataFrame(cormatrix, sheet, startRow = row + 2, startColumn = column, col.names=TRUE, row.names=FALSE)
  lastRow <- row + 2 + nrow(cormatrix)
  lastColumn <- column + ncol(cormatrix)
  # Column Names
  headerCells <- getCells(sheet=sheet, rows=row + 2, columns=column:lastColumn)
  dummy <- styleCells(headerCells, cellStyle=getStyle(wb, "TABLE_COLNAMES"))
  # Row Names
  headerCells <- getCells(sheet=sheet, rows=(row + 3):lastRow, columns=column)
  dummy <- styleCells(headerCells, cellStyle=getStyle(wb, "TABLE_ROWNAMES"))
  #autoSizeColumn(sheet, colIndex=c(column:(ncol(df) + column)))
  xlsx::setColumnWidth(sheet, colIndex=c(column:(ncol(df) + column)), colWidth=20)
  
  # Apply Conditional Formatting
  # CORR Matrix Conditional Formatting
  cells <- getCells(sheet = sheet, rows=(row + 3):lastRow, columns=(column + 1):lastColumn)
  values <- lapply(cells, getCellValue)
  values[is.na(values) | values == ""] <- 0
  hilightbad <- NULL
  for(i in names(values)){
    x <- as.numeric(values[i])
    if (abs(x) >= .66) hilightbad <- c(hilightbad,i)
  }
  dummy <- lapply(names(cells[hilightbad]),
                  function(j) setCellStyle(cells[[j]], getStyle(wb, "CORR_BAD")))
  hilightdanger <- NULL
  for(i in names(values)){
    x <- as.numeric(values[i])
    if(abs(x) > .55 && abs(x) < .66) hilightdanger <- c(hilightdanger,i)
  }
  dummy <- lapply(names(cells[hilightdanger]),
                  function(j) setCellStyle(cells[[j]], getStyle(wb, "CORR_DANGER")))
  
  saveWorkbook(wb, file=excelFile)
  return(list(
    firstRow = row,
    firstColumn = column,
    lastRow = lastRow,
    lastColumn = lastColumn
  ))
} # end of addCorrelations()




#####################################################################
## Add AUC (Area Under ROC) for Train and Test ----------------------
addAUC <- function(wb, sheet, row, column, train.predicted, train.actual, test.predicted=NULL, test.actual=NULL){
  if(is.factor(train.actual)) train.actual <- as.integer(train.actual)
  if(is.factor(test.actual)) test.actual <- as.integer(test.actual)
  addText(sheet, rowIndex=row, colIndex = column, text="Area Under ROC", textStyle=getStyle(wb, "SUBTITLE"))
  addText(sheet, rowIndex=row + 1, colIndex = column + 1, text="Train:")
  addText(sheet, rowIndex=row + 1, colIndex = column + 2, text=auc_roc(preds = train.predicted, actuals=train.actual))
  lastRow <- row + 1
  if(!is.null(test.predicted)){
    addText(sheet, rowIndex=row + 2, colIndex = column + 1, text="Test:")
    addText(sheet, rowIndex=row + 2, colIndex = column + 2, text=auc_roc(preds = test.predicted, actuals=test.actual))
    lastRow <- row + 2
  }
  lastColumn <- column + 2
  saveWorkbook(wb, file=excelFile)
  return(list(
    firstRow = row,
    firstColumn = column,
    lastRow = lastRow,
    lastColumn = lastColumn
  ))
} # end of addAUC()


#####################################################################
## Add Lift Tables for Train and Test -------------------------------
addLiftTables <- function(wb, sheet, row, column, actual, predicted, chartTitle=NULL, cumLift=TRUE, pctResponse=TRUE){
  addText(sheet, rowIndex=row, colIndex = column, text=paste("Lift Table -- ",chartTitle), textStyle=getStyle(wb, "SUBTITLE"))
  lt <- generateLiftTable(actual=actual, predicted=predicted)  
  addDataFrame(as.data.frame(lt), sheet, startRow = row + 2, startColumn = column, col.names=TRUE, row.names=FALSE)
  # Column Names
  lastColumn <- column + ncol(lt) - 1
  headerCells <- getCells(sheet=sheet, rows=row + 2, columns=column:lastColumn)
  dummy <- styleCells(headerCells, cellStyle=getStyle(wb, "TABLE_COLNAMES"))
  if(cumLift){
    filename <- tempfile(pattern = "liftplot", tmpdir=getwd(), fileext=".png")
    thisPlot <- plotCumLift(liftTable = lt, chartTitle = chartTitle)
    png(filename=filename, height=800, width=800, res=250, pointsize=8)
    print(thisPlot)
    dev.off()
    addPicture(file = filename, sheet, scale=1, startRow = row, startColumn=lastColumn + 2)
    file.remove(filename)
    lastColumn <- lastColumn + 3
  }
  if(pctResponse){
    filename <- tempfile(pattern = "responseplot", tmpdir=getwd(), fileext=".png")
    thisPlot <- plotPctResponse(liftTable = lt, chartTitle = chartTitle)
    png(filename=filename, height=800, width=800, res=250, pointsize=8)
    print(thisPlot)
    dev.off()
    addPicture(file = filename, sheet, scale=1, startRow = row, startColumn=lastColumn +2)
    file.remove(filename)
    lastColumn <- lastColumn + 3
  }
  saveWorkbook(wb, file=excelFile)
  lastRow <- row + nrow(lt) + 3
  return(list(
    firstRow = row,
    firstColumn = column,
    lastRow = lastRow,
    lastColumn = lastColumn
  ))
} # end of addLiftTable()



#####################################################################
## Add SQL for scoring the model ------------------------------------
addScoringSql <- function(wb, sheet, row, column, df){
  addText(sheet, rowIndex=row, colIndex = column, text="Model Scoring SQL", textStyle=getStyle(wb, "SUBTITLE"))
  lastRow <- row

  intercept <- df[df$variable == '(Intercept)','Estimate']
  betas <- df[df$variable != '(Intercept)',1:2]
  
  equation <- paste0("\t\t ",intercept, " +\n", 
                     paste0("\t\t ", betas$Estimate, " * COALESCE(", betas$variable, ", 0)", collapse = " +\n"))
  sql <- paste0("SELECT ",
                ifelse(nchar(scoringIndexColumns) > 0,paste0(scoringIndexColumns,",\n\t "),"\n\t "),
                ifelse(nchar(scoringCriteria) > 0, paste0("CASE WHEN ",scoringCriteria," THEN \n\t "),""), 
                "CAST((\n",
                equation,"\n\t ",
                ") AS DECIMAL(10,4)) END as ",scoreVariable,"\n",
                "from ", scoringTable)
  
  # split sql into multiple lines
  sqlLines <- strsplit(sql, "\n", fixed=TRUE)[[1]]
  for(i in 1:length(sqlLines)){
    thisLine <- gsub("\t","    ",sqlLines[i],fixed=TRUE)
    addText(sheet, rowIndex = lastRow + 1 + i, colIndex = column, text=thisLine)
  } # end for(i in 1:length(sqlLines))
  
  saveWorkbook(wb, file=excelFile)
  lastRow <- lastRow + 1 + length(sqlLines)
  return(list(
    firstRow = row,
    firstColumn = column,
    lastRow = lastRow,
    lastColumn = column
  ))
} # end of addScoringSql()

#####################################################################
## Add Variable Importance Plot  ------------------------------------
addImportancePlot <- function(wb, sheet, row, column, df){
  addText(sheet, rowIndex=row, colIndex = column, text="Variable Importance", textStyle=getStyle(wb, "SUBTITLE"))
  df <- df[order(-df$Importance),]
  filename <- tempfile(pattern = "varImp", tmpdir=getwd(), fileext=".png")
  thisPlot <- plotVarImportance(df, chartTitle="Random Forest")
  png(filename=filename, height=1600, width=800, res=250, pointsize=10)
  print(thisPlot)
  dev.off()
  addPicture(file = filename, sheet, scale=1, startRow = row + 2, startColumn=column)
  file.remove(filename)
  saveWorkbook(wb, file=excelFile)
  lastRow <- row + 32
  lastColumn <- column + 5
  return(list(
    firstRow = row,
    firstColumn = column,
    lastRow = lastRow,
    lastColumn = lastColumn
  ))
} # end of addImportancePlot()

#####################################################################
## Add Confusion Matrix  ------------------------------------
addConfusionMatrix <- function(wb, sheet, row, column, actual, predicted, title, subtitle){
  if(is.factor(actual)) actual <- as.integer(actual)
  
  # Find probability cutoff 2 deciles deep
  deciles <- quantile(predicted, probs=seq(0.9, 0.1, -0.10))
  decile.cutoff <- deciles[2] 
  cm <- generateConfusionMatrix(actual=actual, predicted=predicted, cutoff=decile.cutoff)

  lastRow <- row
  lastColumn <- column
  #title
  addText(sheet, rowIndex=lastRow, colIndex = lastColumn, text=title, textStyle=getStyle(wb, "SUBTITLE"))
  lastRow <- lastRow + 1
  lastColumn <- lastColumn + 1
  #train
  addText(sheet, rowIndex=lastRow, colIndex = lastColumn, text=subtitle)
  addText(sheet, rowIndex=lastRow + 3, colIndex = lastColumn, text="Actual")
  addText(sheet, rowIndex=lastRow + 1, colIndex = lastColumn + 2, text="Predicted")
  addDataFrame(as.data.frame.matrix(cm$cm), sheet, startRow = lastRow + 2, startColumn=lastColumn + 1, row.names=TRUE, col.names=TRUE)
  tableCells <- getCells(sheet = sheet, rows=(lastRow+2):(lastRow+4), columns=(lastColumn + 1):(lastColumn + 3))
  for(cell in tableCells) setCellStyle(cell, getStyle(wb, "GRID"))
  lastRow <- lastRow + 4
  lastColumn <- lastColumn + 3
  
  saveWorkbook(wb, file=excelFile)
  return(list(
    firstRow = row,
    firstColumn = column,
    lastRow = lastRow,
    lastColumn = lastColumn
  ))
} # end of addConfusionMatrix()

