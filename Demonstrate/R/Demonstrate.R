Demonstrate <- function(dir, make.AUC.plot=TRUE, AUC.plot.title="Mean AUC By Population Structure and Heritability",
	make.MAE.plot=TRUE, MAE.plot.title="Mean MAE By Population Structure and Heritability") {

	makeFiles <- function(dir) {

		readFiles <- function(dir) {
			setwd(dir)
			files <- (Sys.glob("*.txt"))
			listOfFiles <- lapply(files, function(x) read.table(x, header = TRUE))
			return(listOfFiles)
		}

		createHeritLabel <- function(data) {
			newData <- data
			herit03 <- sapply(data$Name,function(x) grepl("_03_",x))
			herit04 <- sapply(data$Name,function(x) grepl("_04_",x))
			herit06 <- sapply(data$Name,function(x) grepl("_06_",x))
			newData$Herit <- ifelse(herit03,0.3,NA)
			newData$Herit <- ifelse(herit04,0.4,newData$Herit)
			newData$Herit <- ifelse(herit06,0.6,newData$Herit)
			return(newData)
		}

		createStructureLabel <- function(data) {
			newData <- data
			struct <- sapply(data$Name,function(x) grepl("PheHasStruct",x))
			none <- sapply(data$Name,function(x) grepl("PheNPStruct",x))
			newData$Structure <- ifelse(struct,TRUE,NA)
			newData$Structure <- ifelse(none,FALSE,newData$Structure)
			return(newData)	
		}

		createLabels <- function(data) {
			newData <- createHeritLabel(data)
			newNewData <- createStructureLabel(newData)
			return(newNewData)
		}

		myFiles <- readFiles(dir)
		myFiles <- lapply(myFiles, createLabels)

		return(myFiles)

	}

	myFiles <- makeFiles(dir)
	totalDataSet <- myFiles[[1]]
	for (i in 2:length(myFiles)) {
		totalDataSet <- rbind(totalDataSet, myFiles[[i]])
	}

	require(sciplot)

	if (make.AUC.plot) {
		pdf(file=AUC.plot.title)
		lineplot.CI(totalDataSet$Herit,totalDataSet$AUC,totalDataSet$Structure,main=AUC.plot.title,
			xlab="Heritability",ylab="Mean AUC",trace.label="Pop. Structure")
		dev.off()
	}

	if (make.MAE.plot) {
		pdf(file=MAE.plot.title)
		lineplot.CI(totalDataSet$Herit,totalDataSet$MAE,totalDataSet$Structure,main=MAE.plot.title,
			xlab="Heritability",ylab="Mean MAE",trace.label="Pop. Structure")		
		dev.off()
	}

	return(totalDataSet)

}