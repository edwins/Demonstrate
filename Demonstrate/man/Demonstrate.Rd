\name{Demonstrate}
\alias{Demonstrate}
\title{
Demonstrate takes outputs from the Validate known-truth pipeline for iPlant Collaborative and produces a single data set and graphics.}
\description{
Demonstrate is the third step in the Validate known-truth pipeline for iPlant collaborative. It produces a single data set from several Validate results files (produced at iPlantcollaborative.org as well as two graphics showing differences in a GWAS/QTL applications performance under varying heritability and population structure.
}
\usage{
Demonstrate(dir, make.AUC.plot = TRUE, AUC.plot.title = "Mean AUC By Population Structure and Heritability", make.MAE.plot = TRUE, MAE.plot.title = "Mean MAE By Population Structure and Heritability")
}
\value{
Returns a single data set with Heritability and Population Structure variables. Also has the capability to save two graphics.
}
\author{
Dustin A. Landers}
