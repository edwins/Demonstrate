require(getopt)

args <- commandArgs(trailingOnly=TRUE)

options <- matrix(c("dir","a",1,"character",
					"make-AUC-plot","b",0,"integer",
					"AUC-plot-title","c",0,"character",
					"make-MAE-plot","d",0,"integer",
					"MAE-plot-title","e",0,"character",
					"herit-string1","f",0,"character",
					"herit-string2","g",0,"character",
					"herit-string3","h",0,"character",
					"herit-values1","i",0,"character",
					"herit-values2","j",0,"character",
					"herit-values3","k",0,"character",
					"struct-strings1","l",0,"character",
					"struct-strings2","m",0,"character",
					"struct-values1","o",0,"character",
					"struct-values2","p",0,"character"),
		ncol=4,byrow=TRUE)

ret.opts <- getopt(options,args)
myDir <- ret.opts$dir
if (exists(ret.opts$make-AUC-plot)) {
	myMakeAUC <- ret.opts$make-AUC-plot
	print(myMakeAUC)
}

#Demonstrate <- function(dir, make.AUC.plot=TRUE, AUC.plot.title="Mean AUC By Population Structure and Heritability",
#	make.MAE.plot=TRUE, MAE.plot.title="Mean MAE By Population Structure and Heritability",herit.strings=list("_03_","_04_","_06_")
#	,herit.values=list(0.3,0.4,0.6),struct.strings=list("PheHasStruct","PheNPStruct"),struct.values=list(TRUE,FALSE))

#Demonstrate()