library(igraph)
library(readr)
library(xlsx)
library(poweRlaw)
library(ggplot2)
source("custom_funtions.R")

Initial.matrix <- read.csv(file = 'input/adjacencyMatrices/data-starplus-04799-v7/subject_04799_adjacency_matrix_with_adjacencyMeasure_0_1.csv', header = FALSE)
Initial.matrix[is.na(Initial.matrix)] <- 0
matrix <- as.matrix(Initial.matrix)

g <- graph_from_adjacency_matrix(matrix, mode="undirected", weighted=NULL, add.colnames = NULL,
                                 add.rownames = NA) # For undirected networks

dg<-degree_distribution(g,cumulative=F,mode="all")
#jpeg("rplost.jpg")
pdf("rp0lost.pdf")
result<- plot(dg[dg!=0], log="xy",main=paste("degree distribution for ","plotFileName"),xlab="k", ylab="P(k)")

dev.off ()