library(data.table)
library(dplyr)
library(xlsx)

var_names_t <- data.frame(file=character())
vectorNames <- c()

## Read the column names of the data or 561 vector variables on each row

readColNames <- function (){
	if (nrow(var_names_t) == 0) {
		var_names_t <- read.table("./data/features.txt",header=FALSE,sep=" ")
	}
	var_names_t
}

## Read activity lables from the file

readActivityNames <- function() {
	var_act_names_t <- read.table("./data/activity_labels.txt", header=FALSE)
	var_act_names_t
}

## Read names that are more user friendly from a file that was created

ReadMeaningfulNames <- function() {
	var_names_t <- read.xlsx("./data/VariableDesc.xlsx", sheetName="VarDesc", startRow = 1)
	var_names_t <- var_names_t %>% mutate (avg = paste(New.Name,"_Avg", sep="")) ## Append suffix _Avg to set the names for Average data set
	var_names_t
}

applyColNames <- function(dataSet){
	varNames <- readColNames()
	NamesVector <- as.vector(varNames[,2])
	colnames(dataSet) <- NamesVector
	dataSet
}

## Selecet average and standard deviation related columns or variables

SelectMeanStdCols <- function(){
	if (nrow(var_names_t) == 0) {
		var_names_t <- readColNames()
	}
	if (length(vectorNames) == 0 ){
		sel_var_names_t <- var_names_t[grepl("mean|std", var_names_t[,2]),]
		vectorNames <-  as.vector(sel_var_names_t[,2])
	}
	vectorNames
}

## Read Traning data from trainig files and process to select data/columns related  to Average and Std. Variance

readTrainingData <- function() {
	## read data from files

	X_train_t <- read.table("./data/train/X_train.txt",header=FALSE)
	y_train_t <- read.table("./data/train/y_train.txt",header=FALSE)
	subject_train_t <- read.table("./data/train/subject_train.txt",header=FALSE)	
	## apply column names to all three data sets

	X_train_t <- applyColNames(X_train_t)
	names(y_train_t)[1] <- "activityLabels"
	names(subject_train_t)[1] <- "volunteerSubjectId"
	## set descriptive activity names for Y-TRAIN-T set data

	var_act_names_t <- readActivityNames()
	y_train_t[,1] <- sapply(y_train_t[,1], function(y){var_act_names_t[(var_act_names_t$V1 ==y),2]})
	
	## Select only Mean and Std (Standard Variance variables

	vectorNames = SelectMeanStdCols()
	X_sel_train_t <- X_train_t[, vectorNames]
	X_sel_train_t <- cbind(subject_train_t, y_train_t, X_sel_train_t)
	X_sel_train_t
}

## Read Test data from test files and process to select data/columns related related  to Average and Std. Variance

readTestData <- function () {
	## read data from files

	X_test_t <- read.table("./data/test/X_test.txt",header=FALSE)
	y_test_t <- read.table("./data/test/y_test.txt",header=FALSE)
	subject_test_t <- read.table("./data/test/subject_test.txt",header=FALSE)
	## apply column names to all three data sets

	X_test_t <- applyColNames(X_test_t)
	names(y_test_t)[1] <- "activityLabels"
	names(subject_test_t)[1] <- "volunteerSubjectId"
	## set descriptive activity names for Y-TRAIN-T set data

	var_act_names_t <- readActivityNames()
	y_test_t[,1] <- sapply(y_test_t[,1], function(y){var_act_names_t[(var_act_names_t$V1 ==y),2]})

	## Select only Mean and Std (Standard Variance variables

	vectorNames = SelectMeanStdCols()
	X_sel_test_t <- X_test_t[, vectorNames]
	X_sel_test_t <- cbind(subject_test_t, y_test_t, X_sel_test_t)
	X_sel_test_t
}

## Merge both the data sets and compute mean for each variable

MergeData <- function() {
	X_train_t <- readTrainingData()
	X_test_t <- readTestData()
	total_data_t <- rbind(X_train_t,X_test_t)

	var_names_t <- ReadMeaningfulNames() ## Get descriptive names for each of the variables in the data set
	i <- 1

	## Set descriptive variable names for the combined data set
 
	for (colName in var_names_t[,2]) {
		k <- as.character(var_names_t[i,1])
		names(X_train_t)[names(X_train_t)==colName] <- k
		names(total_data_t)[names(total_data_t)==colName] <- k
		i <- i+1
	}
	write.table(total_data_t, file = "./data/TotalData.csv",row.names=FALSE, na="", sep=",")	
	## Calculate average for each column and store in a separate data frame and set descriptive names

	avg_total_data_t <- total_data_t %>% group_by(volunteerSubjectId,activityLabels) %>% summarise_all(funs(mean))  
	i <- 1
	for (colName in var_names_t[,1]) {
		k <- as.character(var_names_t[i,3])
		names(avg_total_data_t)[names(avg_total_data_t)==colName] <- k
		i <- i+1
	}
	write.table(avg_total_data_t, file = "./data/TotalDataAvg.txt",row.names=FALSE, na="", sep=",")	
	dataSets <- list(total_data_t,avg_total_data_t)		## create a list of two data sets to return
	dataSets ## retrun the two data sets
}