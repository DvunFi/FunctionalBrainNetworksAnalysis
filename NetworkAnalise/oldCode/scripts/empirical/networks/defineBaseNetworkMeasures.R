library(igraph)
library(readr)
library(xlsx)
library(poweRlaw)
library(ggplot2)


inputFolderPath <- "../input/adjacencyMatrixes/"
outputFolderPath <- "output/"
listOfDirFilesName <- list.fles(inputFolderPath)
listOfCsvFilesName <- list.files(inputFolderPath)

for(dirName in listOfDirFilesName){
  i<-.05
  j<-1
  listOfCsvFilesName <- list.files(paste(inputFolderPath,dirName,"/", sep = "", collapse = NULL))
  statistic <- matrix( nrow = 0, ncol = 15)  
  colnames(statistic) <- c("V"
                           ,"E"
                           ,"mean_degree"
                           ,"max_conected_componet"
                           ,"assortativity_degree"
                           ,"mean_distance"
                           ,"modularity_cl_louvail"
                           ,"modularity_cl_infomap"
                           #,"modularity_cl_m_optimal"
                           #,"modularity_cl_edge_betweenness"
                           ,"modularity_cl_fast_greedy"
                           ,"modularity_cl_label_prop"
                           #,"modularity_cl_leading_eigen"
                           ,"cl_louvail_info"
                           ,"cl_infomap_info"
                           ,"cl_fast_greedy_info"
                           ,"cl_label_prop_info"
                           ,"csv_file_name")  
   
   for(csvFileName in listOfCsvFilesName){
      
      if(csvFileName != 'info_table.xls'){
        adjacencyMat <- as.matrix(read.csv(paste(inputFolderPath,dirName,"/",csvFileName, sep = "", collapse = NULL),header = FALSE))
        g <- simplify(graph_from_adjacency_matrix(adjacencyMat,mode = c("undirected"),weighted = NULL),remove.loops=TRUE)
      }
      
      if(!is.null(g) && is_simple(g)) {
       
         cl_louvail           <- cluster_louvain(g)
         cl_infomap           <- cluster_infomap(g)
         #cl_m_Optimal         <- cluster_optimal(g)
         #cl_edge_betweenness  <- cluster_edge_betweenness(g)
         cl_fast_greedy       <- cluster_fast_greedy(g)
         cl_label_prop        <- cluster_label_prop(g)
         #cl_leading_eigen <-cluster_leading_eigen(g)
         
         statistic <- rbind(statistic
                           , c(gorder(g)
                               , gsize(g)
                               , mean(degree(g))
                               , max(sizes(clusters(g)))
                               , assortativity_degree(g)
                               , mean_distance(g)
                               , modularity(cl_louvail)
                               , modularity(cl_infomap)
                               #, modularity(cl_m_Optimal)
                               #, modularity(cl_edge_betweenness)
                               , modularity(cl_fast_greedy)
                               , modularity(cl_label_prop)
                               #, modularity(cl_leading_eigen)
                               , paste("com_count:",length(communities(cl_louvail)),"mean_com_size:",mean(sizes(cl_louvail)))
                               , paste("com_count:",length(communities(cl_infomap)),"mean_com_size:",mean(sizes(cl_infomap)))
                               , paste("com_count:",length(communities(cl_fast_greedy)),"mean_com_size:",mean(sizes(cl_fast_greedy)))
                               , paste("com_count:",length(communities(cl_label_prop)),"mean_com_size:",mean(sizes(cl_label_prop)))
                               , csvFileName))
        
        print(csvFileName)
        print(length(E(g)))
        #fc <-NULL
      }
     dg<-degree_distribution(g,cumulative=F,mode="all")
     
     plot(dg[dg!=0], log="xy",main=paste("degree distribution for correlation measure",i),xlab="k", ylab="P(k)")
     dev.copy(jpeg,filename=paste(outputFolderPath,dirName,"_out/plot_deegree_distribution/",dirName,"_degree_distribution_",j,".jpeg",sep = ""))
     dev.off ();
     
     #plot(sizes(cl_louvail),main=paste("com size for correlation measure",i,"(method louvail)"),xlab="com", ylab="com size")    
     plot(sizes(cl_louvail),main=paste("com size for correlation measure",i,"(method louvail)"),xlab="com", ylab="com size")    
     dev.copy(jpeg,filename=paste(outputFolderPath,dirName,"_out/plot_louvail/",dirName,"(method louvail)_com_size_",j,".jpeg",sep = ""))
     dev.off ();
     
     #plot(sizes(cl_infomap),main=paste("com size for correlation measure",i,"(method infomap)"),xlab="com", ylab="com size")    
     plot(sizes(cl_infomap),main=paste("com size for correlation measure",i,"(method infomap)"),xlab="com", ylab="com size")    
     dev.copy(jpeg,filename=paste(outputFolderPath,dirName,"_out/plot_infomap/",dirName,"(method louvail)_com_size_",j,".jpeg",sep = ""))
     dev.off ();
     
     #plot(sizes(cl_fast_greedy),main=paste("com size for correlation measure",i,"(method fast_greedy)"),xlab="com", ylab="com size")    
     plot(sizes(cl_fast_greedy),main=paste("com size for correlation measure",i,"(method fast_greedy)"),xlab="com", ylab="com size")    
     dev.copy(jpeg,filename=paste(outputFolderPath,dirName,"_out/plot_fast_greedy/",dirName,"(method fast_greedy)_com_size_",j,".jpeg",sep = ""))
     dev.off ();
     
     #plot(sizes(cl_label_prop),main=paste("com size for correlation measure",i,"(method label prop)"),xlab="com", ylab="com size")    
     plot(sizes(cl_label_prop),main=paste("com size for correlation measure",i,"(method label prop)"),xlab="com", ylab="com size")    
     dev.copy(jpeg,filename=paste(outputFolderPath,dirName,"_out/plot_label_prop/",dirName,"(method label_prop)_com_size_",j,".jpeg",sep = ""))
     dev.off ();
     g <- NULL
     i<-i+0.05
     j<-j+1
   }
  
  write.xlsx(statistic, paste(outputFolderPath,dirName,"_out/",dirName,"_","defineAdjacencyMeasure_result.xlsx", sep = "", collapse = NULL))
  
}