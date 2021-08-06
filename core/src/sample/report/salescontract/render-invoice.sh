#!/usr/bin/Rscript
library("knitr")
library("rmarkdown")
args=commandArgs(trailingOnly=TRUE)
rmarkdown::render("invoice.Rmd", output_file=paste(args[1], ".pdf", sep=""), params = list(invoiceNumber=args[1]))
