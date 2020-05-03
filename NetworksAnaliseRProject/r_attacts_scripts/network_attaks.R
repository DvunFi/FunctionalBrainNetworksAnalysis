library(igraph)
library(readr)
library(xlsx)
library(poweRlaw)
library(ggplot2)

attacks.random_attack_on_vertexs <- function(outputDirpath, inputDirPath, subjectName, listOfCsvFilesName, count_of_delete_v, datePostfix){
  print("attacks.random_attack_on_vertexs start")
  print(subjectName)  
  
  print("create output dir for subject")
  outDirPostfix <- datePostfix
  outDirName <- paste(outputDirpath, "/", subjectName, "_out_", outDirPostfix, "/random_attack_on_vertexs/", sep = "")
  dir.create(outDirName,recursive = TRUE)
  
  print("create basic output dir for subject plots")
  outPlotDirName <- paste(outputDirpath,"/", subjectName, "_out_", outDirPostfix,"/random_attack_on_vertexs/plots",sep = "")
  dir.create(outPlotDirName)
  
  for(csvFileName in listOfCsvFilesName){ 
    print(paste("start calculate ",csvFileName))
    Initial.matrix <- read.csv(file = paste(inputDirPath,subjectName,'/',csvFileName,sep = ""), header = FALSE)
    Initial.matrix[is.na(Initial.matrix)] <- 0
    matrix <- as.matrix(Initial.matrix)
    
    g <- simplify(graph_from_adjacency_matrix(matrix, mode="undirected", weighted=NULL, add.colnames = NULL, add.rownames = NA))
    
    outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
    
    if(!is.null(g) && is_simple(g)) {
      outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
      print("create output exel file for statistic")
      outExelFile <- paste(outDirName, outPutFileName,"_random_attacks_result.xlsx", sep="")
      file.create(outExelFile)
      
      #define DF for results
      statistic <- matrix( nrow = 0, ncol = 12)  
      colnames(statistic) <- c("V" ,"E", "v_deleted" ,"v_min_bc" ,"v_max_bc" ,"modularity_cl_louvail"  ,"cl_louvail_info"
                               ,"max_conected_componet" ,"assortativity_degree" ,"mean_degree" ,"mean_distance" ,"csv_file_name")  
      
      v_deleted_sum <- 0
      #insert statistic to df (start statistic)
      #=statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
      #plot
      #attacks.share_of_mrc_to_deleted(g, outPlotDirName, outPutFileName , v_deleted_sum)
     
       while (max(sizes(clusters(g)))/gorder(g)>0.05) {
        g<-delete.vertices(g,sample(V(g),count_of_delete_v))
        v_deleted_sum<-v_deleted_sum+count_of_delete_v
        #insert statistic to df (start statistic)
        statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
        #plot
        attacks.share_of_mrc_to_deleted(g, outPlotDirName, outPutFileName , v_deleted_sum)
        print(paste("v_deleted", v_deleted_sum))
       }
      
      write.xlsx(statistic, outExelFile)
      
      statistic <- NULL
    }else print("fuuuuucked up")
  }
  
  print("attacks.random_attack_on_vertexs end")
}

attacks.target_attack_on_vertexs_max_bc <- function(outputDirpath, inputDirPath, subjectName, listOfCsvFilesName, count_of_delete_v, datePostfix){
  print("attacks.target_attack_on_vertexs_max_bc start")
  print(subjectName)  
  
  print("create output dir for subject")
  outDirPostfix <- datePostfix
  outDirName <- paste(outputDirpath, "/", subjectName, "_out_", outDirPostfix, "/target_attack_on_vertexs_max_bc/", sep = "")
  dir.create(outDirName,recursive = TRUE)
  
  print("create basic output dir for subject plots")
  outPlotDirName <- paste(outputDirpath,"/", subjectName, "_out_", outDirPostfix,"/target_attack_on_vertexs_max_bc/plots",sep = "")
  dir.create(outPlotDirName)
  
  for(csvFileName in listOfCsvFilesName){ 
    print(paste("start calculate ",csvFileName))
    Initial.matrix <- read.csv(file = paste(inputDirPath,subjectName,'/',csvFileName,sep = ""), header = FALSE)
    Initial.matrix[is.na(Initial.matrix)] <- 0
    matrix <- as.matrix(Initial.matrix)
    
    g <- simplify(graph_from_adjacency_matrix(matrix, mode="undirected", weighted=NULL, add.colnames = NULL, add.rownames = NA))
    
    outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
    
    if(!is.null(g) && is_simple(g)) {
      outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
      print("create output exel file for statistic")
      outExelFile <- paste(outDirName, outPutFileName,"_target_attack_on_vertexs_max_bc_attacks_result.xlsx", sep="")
      file.create(outExelFile)
      
      #define DF for results
      statistic <- matrix( nrow = 0, ncol = 12)  
      colnames(statistic) <- c("V" ,"E", "v_deleted" ,"v_min_bc" ,"v_max_bc" ,"modularity_cl_louvail"  ,"cl_louvail_info"
                               ,"max_conected_componet" ,"assortativity_degree" ,"mean_degree" ,"mean_distance" ,"csv_file_name")  
      
      v_deleted_sum <- 0
      #insert statistic to df (start statistic)
      statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
      #plot
      #attacks.share_of_mrc_to_deleted(g, outPlotDirName, outPutFileName , v_deleted_sum)
      
      while (max(sizes(clusters(g)))/gorder(g)>0.05) {
        v_ids<-seq(V(g))
        v_bc <- betweenness(g, V(g), directed = FALSE)
        df_id_bc<-data.frame(v_bc,v_ids)
        df_id_bc<-df_id_bc[with(df_id_bc, order(-v_bc,v_ids)), ]
        ordered_v_ids<-df_id_bc[[2]]
        g<-delete.vertices(g,ordered_v_ids[1:count_of_delete_v])
        #insert statistic to df (start statistic)
        statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
        #plot
        attacks.share_of_mrc_to_deleted(g, outPlotDirName, outPutFileName , v_deleted_sum)
        v_deleted_sum<-v_deleted_sum+count_of_delete_v
        print(paste("v_deleted", v_deleted_sum))
        df_id_bc <- NULL
      }
     
      
      write.xlsx(statistic, outExelFile)
      
      statistic <- NULL
    }else print("fuuuuucked up")
  }
  
  print("attacks.target_attack_on_vertexs_max_bc end")
}

attacks.target_attack_on_eges_max_bc <- function(outputDirpath, inputDirPath, subjectName, listOfCsvFilesName, count_of_delete_v, datePostfix){
  print("attacks.target_attack_on_eges_max_bc start")
  print(subjectName)  
  
  print("create output dir for subject")
  outDirPostfix <- datePostfix
  outDirName <- paste(outputDirpath, "/", subjectName, "_out_", outDirPostfix, "/target_attack_on_eges_max_bc/", sep = "")
  dir.create(outDirName,recursive = TRUE)
  
  print("create basic output dir for subject plots")
  outPlotDirName <- paste(outputDirpath,"/", subjectName, "_out_", outDirPostfix,"/target_attack_on_eges_max_bc/plots",sep = "")
  dir.create(outPlotDirName)
  
  for(csvFileName in listOfCsvFilesName){ 
    print(paste("start calculate ",csvFileName))
    Initial.matrix <- read.csv(file = paste(inputDirPath,subjectName,'/',csvFileName,sep = ""), header = FALSE)
    Initial.matrix[is.na(Initial.matrix)] <- 0
    matrix <- as.matrix(Initial.matrix)
    
    g <- simplify(graph_from_adjacency_matrix(matrix, mode="undirected", weighted=NULL, add.colnames = NULL, add.rownames = NA))
    
    outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
    
    if(!is.null(g) && is_simple(g)) {
      outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
      print("create output exel file for statistic")
      outExelFile <- paste(outDirName, outPutFileName,"_target_attack_on_eges_max_bc_attacks_result.xlsx", sep="")
      file.create(outExelFile)
      
      #define DF for results
      statistic <- matrix( nrow = 0, ncol = 12)  
      colnames(statistic) <- c("V" ,"E", "v_deleted" ,"v_min_bc" ,"v_max_bc" ,"modularity_cl_louvail"  ,"cl_louvail_info"
                               ,"max_conected_componet" ,"assortativity_degree" ,"mean_degree" ,"mean_distance" ,"csv_file_name")  
      
      v_deleted_sum <- 0
      #insert statistic to df (start statistic)
      statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
      #plot
      #attacks.share_of_mrc_to_deleted(g, outPlotDirName, outPutFileName , v_deleted_sum)
      
      while (max(sizes(clusters(g)))/gorder(g)>0.05) {
        e_ids<-seq(E(g))
        e_bc <- edge_betweenness(g, e=E(g), directed = FALSE, weights = NULL)
        df_id_bc<-data.frame(e_bc, e_ids)
        df_id_bc<-df_id_bc[with(df_id_bc, order(-e_bc,e_ids)), ]
        ordered_v_ids<-df_id_bc[[2]]
        g<-delete.vertices(g,ordered_v_ids[1:count_of_delete_v])
        #insert statistic to df (start statistic)
        statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
        #plot
        attacks.share_of_mrc_to_deleted(g, outPlotDirName, outPutFileName , v_deleted_sum)
        v_deleted_sum<-v_deleted_sum+count_of_delete_v
        print(paste("v_deleted", v_deleted_sum))
        df_id_bc <- NULL
      }
      
      write.xlsx(statistic, outExelFile)
      
      statistic <- NULL
    }else print("fuuuuucked up")
  }
  
  print("attacks.target_attack_on_vertexs_max_bc end")
}

attacks.target_attack_on_vertexs_max_degree <- function(outputDirpath, inputDirPath, subjectName, listOfCsvFilesName, count_of_delete_v, datePostfix){
  print("attacks.target_attack_on_vertexs_max_degree start")
  print(subjectName)  
  
  print("create output dir for subject")
  outDirPostfix <- datePostfix
  outDirName <- paste(outputDirpath, "/", subjectName, "_out_", outDirPostfix, "/target_attack_on_vertexs_max_degree/", sep = "")
  dir.create(outDirName,recursive = TRUE)
  
  print("create basic output dir for subject plots")
  outPlotDirName <- paste(outputDirpath,"/", subjectName, "_out_", outDirPostfix,"/target_attack_on_vertexs_max_degree/plots",sep = "")
  dir.create(outPlotDirName)
  
  
  for(csvFileName in listOfCsvFilesName){ 
    print(paste("start calculate ",csvFileName))
    Initial.matrix <- read.csv(file = paste(inputDirPath,subjectName,'/',csvFileName,sep = ""), header = FALSE)
    Initial.matrix[is.na(Initial.matrix)] <- 0
    matrix <- as.matrix(Initial.matrix)
    
    g <- simplify(graph_from_adjacency_matrix(matrix, mode="undirected", weighted=NULL, add.colnames = NULL, add.rownames = NA))
    
    outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
    
    if(!is.null(g) && is_simple(g)) {
      outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
      print("create output exel file for statistic")
      outExelFile <- paste(outDirName, outPutFileName,"_target_attack_on_vertexs_max_degree_attacks_result.xlsx", sep="")
      file.create(outExelFile)
      
      #define DF for results
      statistic <- matrix( nrow = 0, ncol = 12)  
      colnames(statistic) <- c("V" ,"E", "v_deleted" ,"v_min_bc" ,"v_max_bc" ,"modularity_cl_louvail"  ,"cl_louvail_info"
                               ,"max_conected_componet" ,"assortativity_degree" ,"mean_degree" ,"mean_distance" ,"csv_file_name")  
      
      v_deleted_sum <- 0
      #insert statistic to df (start statistic)
      statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
      #plot
      #attacks.share_of_mrc_to_deleted(g, outPlotDirName, outPutFileName , v_deleted_sum)
      
      while (max(sizes(clusters(g)))/gorder(g)>0.05) {
        v_ids <- seq(V(g))
        v_degrees <- degree(g)
        df_v_ids_max_k<-data.frame(v_degrees,v_ids)
        df_v_ids_max_k<-df_v_ids_max_k[with(v_degrees, order(-v_degrees,v_ids)), ]
        v_for_delete<-df_v_ids_max_k[[2]]
        g<-delete.vertices(g,v_for_delete[1:count_of_delete_v])
        #insert statistic to df (start statistic)
        statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
        #plot
        attacks.share_of_mrc_to_deleted(g, outPlotDirName, outPutFileName , v_deleted_sum)
        v_deleted_sum<-v_deleted_sum+count_of_delete_v
        print(paste("v_deleted", v_deleted_sum))
        df_v_ids_max_k <- NULL
      }
      
      write.xlsx(statistic, outExelFile)
      
      statistic <- NULL
    }else print("fuuuuucked up")
  }
  
  print("attacks.target_attack_on_vertexs_max_degree end")
}

#insert statistic to df
#df structure("V" ,"E", "v_deleted" ,"v_min_bc" ,"v_max_bc" ,"modularity_cl_louvail"  ,"cl_louvail_info"
#  ,"max_conected_componet" ,"assortativity_degree" ,"mean_degree" ,"mean_distance" ,"csv_file_name")  
attacks.insert_metrics_to_df <- function(g, df, v_deleted_sum, csv_file_name){
  print("attacks.insert_metrics_to_df(...)")
  v_bc <- betweenness(g, V(g), directed = FALSE)
  cl_louvail <- cluster_louvain(g)

  return(rbind(df, c(gorder(g), gsize(g), v_deleted_sum, min(v_bc), max(v_bc), modularity(cl_louvail)
              , paste("com_count:",length(communities(cl_louvail)),"mean_com_size:",mean(sizes(cl_louvail)))
              , max(sizes(clusters(g))), assortativity_degree(g), mean(degree(g)),  mean_distance(g), csv_file_name)))
}

attacks.share_of_mrc_to_deleted <- function(graph, basicOutputPlotsPath, plotFileName, v_deleted_sum){
  print("attacks.share_of_mrc_to_deleted(...)")
  outGraphDistDirPath <- paste(basicOutputPlotsPath, "/graph_mrc_to_deleted", sep = "")
  
  if(!dir.exists(outGraphDistDirPath)){
    dir.create(outGraphDistDirPath)
  }
  pdf(paste(outGraphDistDirPath,"/",plotFileName,"_graph_mrc_to_deleted_plot_","deleted_",v_deleted_sum,".pdf",sep = ""))
  
  plot(v_deleted_sum/V(graph) ,max(sizes(clusters(graph)))/V(graph) ,xlab="share of deleted vertices", 
       ylab="share of vertices owned MRC", xlim=c(0,1),ylim = c(0,1))
}