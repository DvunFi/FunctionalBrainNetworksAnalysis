library(igraph)
library(readr)
library(xlsx)

inputFolderPath <- "inputResources/adjacencyMatrixes/"
listOfCsvFilesName <- list.files(inputFolderPath)

statistic <- matrix( nrow = 0, ncol = 8)  
colnames(statistic) <- c("V", "E","mean_degree", "max_conected_componet", "modularity","comm_count","mean_comm_size","csv_file_name")  

for(csvFileName in listOfCsvFilesName){
  
  if(csvFileName != 'info_table.xls'){
    adjacencyMat <- as.matrix(read.csv(paste(inputFolderPath,csvFileName, sep = "", collapse = NULL),header = FALSE))
    g <- simplify(graph_from_adjacency_matrix(adjacencyMat,mode = c("undirected"),weighted = NULL),remove.loops=TRUE)
  }
  
  if(!is.null(g) && is_simple(g)) {
    fc <-cluster_infomap(g)
    statistic <- rbind(statistic
                       , c(gorder(g)
                       , gsize(g)
                       , mean(degree(g))
                       , max(sizes(clusters(g)))
                       , modularity(fc)
                       , length(communities(fc))
                       , mean(sizes(fc))
                       , csvFileName))
    
    print(csvFileName)
    print(length(E(g)))
    fc <-NULL
  }
  
  g <- NULL
}

write.xlsx(statistic, "defineAdjacencyMeasure_result.xlsx")
