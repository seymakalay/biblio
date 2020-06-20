
#https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
#https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html
rm(list=ls())
library(devtools); library( dplyr ); library(stringr); library(tidyr);library(plyr)
library(RTextTools); library(data.table);library(xtable);library(ggrepel)
library(bibliometrix); library(ggplot2);library(kableExtra)
library(devtools);library(kableExtra)
#biblioshiny()

M <- convert2df("savedrecs.370.txt", dbsource = "wos", format = "plaintext") 
M <-  M[!is.na(M$PY), ] #REMOVE WHEN PUBLISH YEAR IS NA (PY=NA) TO CONSIDER ONLY PUBLISH PAPER

class(M)

#edges.min is an integer. It indicates the min frequency of edges between two vertices. If
#edge.min=0, all edges are plotted.
# REPLACE edge.min=0, WITH n= 10 to see the 10 nodes

my.ntwrk  <- function(M, anlys, ntwrk, ttle){  
  NetMatrix <- biblioNetwork(M, analysis = anlys, network = ntwrk, sep = ";", shortlabel = TRUE)
  # Plot the network
  networkPlot(NetMatrix,  edges.min = 0, Title = ttle,
              type = "fruchterman", size=T, remove.multiple = FALSE, labelsize = 0.7, edgesize = 5)
  
}

#co-citation network
my.ntwrk(M,"co-citation","references","")
#authors’ collaboration network
my.ntwrk(M,"collaboration","authors","")
#keyword co-occurrences network
my.ntwrk(M, "co-occurrences", "keywords","")


my.ntwrk(M, "coupling", "authors")
