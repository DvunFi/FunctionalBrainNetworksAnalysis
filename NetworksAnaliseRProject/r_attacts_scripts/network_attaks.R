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
  
  print("create basic output dir for csv plots")
  outPlotDirName <- paste(outputDirpath,"/", subjectName, "_out_", outDirPostfix,"/random_attack_on_vertexs/plots/",sep = "")
  print(outPlotDirName)
  dir.create(outPlotDirName, recursive = TRUE)
  
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
    
      loaded_graph_v_count <- gorder(g)
      x_for_plot <- list()
      y_for_plot <- list()
      
      print(paste("max(sizes(clusters(g)))/loaded_graph_v_count at start :", max(sizes(clusters(g)))/loaded_graph_v_count))
      
      if(max(sizes(clusters(g)))/loaded_graph_v_count > 0.05 ){
        while (max(sizes(clusters(g)))/loaded_graph_v_count > 0.05 && v_deleted_sum+count_of_delete_v <= loaded_graph_v_count) {
          g <- delete.vertices(g,sample(V(g),count_of_delete_v))
          v_deleted_sum <- v_deleted_sum+count_of_delete_v
          
          #insert statistic to df (start statistic)
          statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
          #plot
          x_for_plot[length(x_for_plot)+1] <- v_deleted_sum/loaded_graph_v_count
          y_for_plot[length(y_for_plot)+1] <- max(sizes(clusters(g)))/loaded_graph_v_count
        
          print(paste("v_deleted", v_deleted_sum, Sys.time()))
        }
      }else{
        print("max(sizes(clusters(g)))/loaded_graph_v_count < 0.05 !")
        statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
      }
      print(paste("max(sizes(clusters(g)))/loaded_graph_v_count at end :", max(sizes(clusters(g)))/loaded_graph_v_count))
      
      attacks.share_of_mrc_to_deleted(x_for_plot, y_for_plot , outPlotDirName, outPutFileName , v_deleted_sum, loaded_graph_v_count,paste("random attack on V for", outPutFileName), "share of deleted vertices" , "share of vertices owned MRC" )
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
  outPlotDirName <- paste(outputDirpath,"/", subjectName, "_out_", outDirPostfix,"/target_attack_on_vertexs_max_bc/plots/",sep = "")
  dir.create(outPlotDirName, recursive = TRUE)
  
  for(csvFileName in listOfCsvFilesName){ 
    print(paste("start calculate ",csvFileName))
    Initial.matrix <- read.csv(file = paste(inputDirPath,subjectName,'/',csvFileName,sep = ""), header = FALSE)
    Initial.matrix[is.na(Initial.matrix)] <- 0
    matrix <- as.matrix(Initial.matrix)
    
    g <- simplify(graph_from_adjacency_matrix(matrix, mode="undirected", weighted=NULL, add.colnames = NULL, add.rownames = NA))
    
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
      
      loaded_graph_v_count <- gorder(g)
      x_for_plot <- list()
      y_for_plot <- list()
      
      print(paste("max(sizes(clusters(g)))/loaded_graph_v_count at start :", max(sizes(clusters(g)))/loaded_graph_v_count))
      
      if(max(sizes(clusters(g)))/loaded_graph_v_count > 0.05 ){
        while (max(sizes(clusters(g)))/loaded_graph_v_count > 0.05 && v_deleted_sum+count_of_delete_v <= loaded_graph_v_count) {
          v_ids<-seq(V(g))
          v_bc <- betweenness(g, V(g), directed = FALSE)
          df_id_bc <- data.frame(v_bc,v_ids)
          df_id_bc <- df_id_bc[with(df_id_bc, order(-v_bc,v_ids)), ]
          ordered_v_ids <- df_id_bc[[2]]
          g <- delete.vertices(g,ordered_v_ids[1:count_of_delete_v])
        
          #insert statistic to df (start statistic)
          statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
        
          #plot
          x_for_plot[length(x_for_plot)+1] <-  v_deleted_sum/loaded_graph_v_count
          y_for_plot[length(y_for_plot)+1] <- max(sizes(clusters(g)))/loaded_graph_v_count
        
          v_deleted_sum<-v_deleted_sum+count_of_delete_v
          print(paste("v_deleted", v_deleted_sum, Sys.time()))
          df_id_bc <- NULL
        }
      }else {
        print("max(sizes(clusters(g)))/loaded_graph_v_count < 0.05 !")
        statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName) 
      }
      
      print(paste("max(sizes(clusters(g)))/loaded_graph_v_count at end :", max(sizes(clusters(g)))/loaded_graph_v_count))
      print(paste("attacks.target_attack_on_vertexs_max_bc fineshed for:", csvFileName))
      
      attacks.share_of_mrc_to_deleted(x_for_plot, y_for_plot , outPlotDirName, outPutFileName , v_deleted_sum, loaded_graph_v_count,paste("target attack on V_BC for", outPutFileName), "share of deleted vertices" , "share of vertices owned MRC" )
      write.xlsx(statistic, outExelFile)
      statistic <- NULL
    }else print("fuuuuucked up")
  }
  
  print("attacks.target_attack_on_vertexs_max_bc end")
}

attacks.target_attack_on_eges_max_bc <- function(outputDirpath, inputDirPath, subjectName, listOfCsvFilesName, count_of_delete_e, datePostfix){
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
      colnames(statistic) <- c("V" ,"E", "e_deleted" ,"e_min_bc" ,"e_max_bc" ,"modularity_cl_louvail"  ,"cl_louvail_info"
                               ,"max_conected_componet" ,"assortativity_degree" ,"mean_degree" ,"mean_distance" ,"csv_file_name")  
      
      e_deleted_sum <- 0
      #insert statistic to df (start statistic)
      statistic <- attacks.insert_metrics_to_df2(g, statistic, e_deleted_sum, csvFileName)
      
      loaded_graph_v_count <- gorder(g)
      loaded_graph_e_count <- gsize(g)
      x_for_plot <- list()
      y_for_plot <- list()
      print(paste("max(sizes(clusters(g)))/loaded_graph_v_count at start :", max(sizes(clusters(g)))/loaded_graph_v_count))
      
      if(max(sizes(clusters(g)))/loaded_graph_v_count > 0.05 ){
        while (max(sizes(clusters(g)))/gorder(g)>0.05 && e_deleted_sum+count_of_delete_e <= loaded_graph_e_count) {
          e_ids <- seq(E(g))
          e_bc <- edge_betweenness(g, e=E(g), directed = FALSE, weights = NULL)
          df_id_bc <- data.frame(e_bc, e_ids)
          df_id_bc <- df_id_bc[with(df_id_bc, order(-e_bc,e_ids)), ]
          ordered_e_ids <- df_id_bc[[2]]
          g <- delete.edges(g,ordered_e_ids[1:count_of_delete_e])
          
          #plot
          x_for_plot[length(x_for_plot)+1] <- e_deleted_sum/loaded_graph_e_count
          y_for_plot[length(y_for_plot)+1] <- max(sizes(clusters(g)))/loaded_graph_v_count
          
          e_deleted_sum<-e_deleted_sum+count_of_delete_e
          #insert statistic to df (start statistic)
          statistic <- attacks.insert_metrics_to_df2(g, statistic, e_deleted_sum, csvFileName)
          
          print(paste("e_deleted", e_deleted_sum, Sys.time()))
          df_id_bc <- NULL
        }
      } else {
        print("max(sizes(clusters(g)))/loaded_graph_v_count < 0.05 !")
        statistic <- attacks.insert_metrics_to_df2(g, statistic, e_deleted_sum, csvFileName) 
      }
      
      print(paste("max(sizes(clusters(g)))/loaded_graph_v_count at end :", max(sizes(clusters(g)))/loaded_graph_v_count))
      print(paste("attacks.target_attack_on_e_max_bc fineshed for:", csvFileName))
      
      attacks.share_of_mrc_to_deleted(x_for_plot, y_for_plot, outPlotDirName, outPutFileName , e_deleted_sum, loaded_graph_v_count,paste("target attack on E_BC for", outPutFileName) , "share of deleted eges" , "share of vertices owned MRC")
      
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
    
    v_deleted_sum <- 0
    #insert statistic to df (start statistic)
    
    
    loaded_graph_v_count <- gorder(g)
    x_for_plot <- list()
    y_for_plot <- list()
    
    print(paste("max(sizes(clusters(g)))/loaded_graph_v_count at start :", max(sizes(clusters(g)))/loaded_graph_v_count))
    
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
      
      if(max(sizes(clusters(g)))/loaded_graph_v_count > 0.05 ){
        while (max(sizes(clusters(g)))/gorder(g)>0.05 && v_deleted_sum+count_of_delete_v <= loaded_graph_v_count) {
          v_ids <- seq(V(g))
          v_degrees <- degree(g)
          df_v_ids_max_k<-data.frame(v_degrees,v_ids)
          df_v_ids_max_k<-df_v_ids_max_k[with(df_v_ids_max_k, order(-v_degrees,v_ids)), ]
          v_for_delete<-df_v_ids_max_k[[2]]
          g<-delete.vertices(g,v_for_delete[1:count_of_delete_v])
          #insert statistic to df (start statistic)
          statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName)
        
          v_deleted_sum<-v_deleted_sum+count_of_delete_v
          print(paste("v_deleted", v_deleted_sum))
          
          x_for_plot[length(x_for_plot)+1] <-  v_deleted_sum/loaded_graph_v_count
          y_for_plot[length(y_for_plot)+1] <- max(sizes(clusters(g)))/loaded_graph_v_count
          
          df_v_ids_max_k <- NULL
        }
      }else {
        print("max(sizes(clusters(g)))/loaded_graph_v_count < 0.05 !")
        statistic <- attacks.insert_metrics_to_df(g, statistic, v_deleted_sum, csvFileName) 
      }
      
      write.xlsx(statistic, outExelFile)
      attacks.share_of_mrc_to_deleted(x_for_plot, y_for_plot , outPlotDirName, outPutFileName , v_deleted_sum, loaded_graph_v_count,paste("attack on v_max_k for", outPutFileName), "share of deleted vertices" , "share of vertices owned MRC" )
     
      print(paste("max(sizes(clusters(g)))/loaded_graph_v_count at end :", max(sizes(clusters(g)))/loaded_graph_v_count))
      print(paste("attacks.target_attack_on_vertexs_max_bc fineshed for:", csvFileName))
      
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

attacks.insert_metrics_to_df2 <- function(g, df, e_deleted_sum, csv_file_name){
  print("attacks.insert_metrics_to_df2(...)")
  e_bc <-  edge_betweenness(g, e=E(g), directed = FALSE, weights = NULL)
  cl_louvail <- cluster_louvain(g)
  
  return(rbind(df, c(gorder(g), gsize(g), e_deleted_sum, min(e_bc), max(e_bc), modularity(cl_louvail)
                     , paste("com_count:",length(communities(cl_louvail)),"mean_com_size:",mean(sizes(cl_louvail)))
                     , max(sizes(clusters(g))), assortativity_degree(g), mean(degree(g)),  mean_distance(g), csv_file_name)))
}

attacks.share_of_mrc_to_deleted <- function(x, y, basicOutputPlotsPath, plotFileName, v_deleted_sum, graph_v_count, main_name, x_name, y_name){
  print("attacks.share_of_mrc_to_deleted(...)")
  outGraphDistDirPath <- paste(basicOutputPlotsPath, "/graph_mrc_to_deleted", sep = "")
  
  if(!dir.exists(outGraphDistDirPath)){
    dir.create(outGraphDistDirPath)
  }
  pdf(paste(outGraphDistDirPath,"/",plotFileName,"_graph_mrc_to_deleted_plot_","deleted_",v_deleted_sum,".pdf",sep = ""))

  plot(x , y, main = main_name, xlab = x_name, 
      ylab = y_name, xlim=c(0,1),ylim = c(0,1))
  
  #plot(x , y, main = main_name, xlab="share of deleted vertices", 
   #    ylab="share of vertices owned MRC", xlim=c(0,1),ylim = c(0,1))
  dev.off ()
}