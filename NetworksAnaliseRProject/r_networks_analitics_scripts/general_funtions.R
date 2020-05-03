library(igraph)
library(readr)
library(xlsx)
library(poweRlaw)
library(ggplot2)

  network_analise.define_non_overlap_com_structure <- function(outputDirpath, inputDirPath, subjectName, listOfCsvFilesName){
  print("function.define_non_overlap_com_structure started")
  print(subjectName)  
  
  #define DF for results
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
    
    print("create output dir for subject")
    outDirPostfix <- gsub(" ","_",gsub(":","_",gsub("-","_",Sys.time())))
    outDirName <- paste(outputDirpath, "/", subjectName, "_out_", outDirPostfix, "/not_overlap_com_detection/", sep = "")
    dir.create(outDirName,recursive = TRUE)
    
    print("create basic output dir for subject plots")
    outPlotDirName <- paste(outputDirpath,"/", subjectName, "_out_", outDirPostfix,"/plots",sep = "")
    dir.create(outPlotDirName)
    
    print("create output exel file for statistic")
    outExelFile <- paste(outDirName, subjectName,"defineAdjacencyMeasure_result.xlsx", sep="")
    file.create(outExelFile)
    
    for(csvFileName in listOfCsvFilesName){
      Initial.matrix <- readr(file = paste(inputDirPath,subjectName,'/',csvFileName,sep = ""), header = FALSE)
      Initial.matrix[is.na(Initial.matrix)] <- 0
      matrix <- as.matrix(Initial.matrix)
      
      g <- simplify(graph_from_adjacency_matrix(matrix, mode="undirected", weighted=NULL, add.colnames = NULL, add.rownames = NA))
      
      outPutFileName <- gsub(".csv", "", gsub("_adjacency_matrix", "", csvFileName))
      networks_analise.plot_degree_distribution(g, outPlotDirName, outPutFileName)
      
      if(!is.null(g) && is_simple(g)) {
        print("cluster_louvain")
        cl_louvail <- cluster_louvain(g)
        print("cluster_infomap")
        cl_infomap  <- cluster_infomap(g)
        print("cluster_fast_greedy")
        cl_fast_greedy <- cluster_fast_greedy(g)
        print("cluster_label_prop")
        cl_label_prop <- cluster_label_prop(g)
        
        statistic <- rbind(statistic
                           , c(gorder(g)
                               , gsize(g)
                               , mean(degree(g))
                               , max(sizes(clusters(g)))
                               , assortativity_degree(g)
                               , mean_distance(g)
                               , modularity(cl_louvail)
                               , modularity(cl_infomap)
                               , modularity(cl_fast_greedy)
                               , modularity(cl_label_prop)
                               , paste("com_count:",length(communities(cl_louvail)),"mean_com_size:",mean(sizes(cl_louvail)))
                               , paste("com_count:",length(communities(cl_infomap)),"mean_com_size:",mean(sizes(cl_infomap)))
                               , paste("com_count:",length(communities(cl_fast_greedy)),"mean_com_size:",mean(sizes(cl_fast_greedy)))
                               , paste("com_count:",length(communities(cl_label_prop)),"mean_com_size:",mean(sizes(cl_label_prop)))
                               , csvFileName))
        
        print(csvFileName)
        print(length(E(g)))
        print("rbind done")
      }else{
        print(csvFileName)
        print("fuccccck")
      }
    
    }
    #file.create(paste(outputDirpath,subjectName,"_out/","not_overlap_com_detection/",subjectName,"defineAdjacencyMeasure_result.xlsx", sep = "", collapse = NULL))
    write.xlsx(statistic, outExelFile)
    print("function.define_non_overlap_com_structure finished")

}

network_analise.plot_degree_distribution <- function(graph, basicOutputPlotsPath, plotFileName){
  #degree distribution
  print("function.plot_degree_distribution(...)")
  outGraphDistDirPath <- paste(basicOutputPlotsPath, "/graph_degree_distribution", sep = "")
  
  if(!dir.exists(outGraphDistDirPath)){
    dir.create(outGraphDistDirPath)
  }
  
  dg<-degree_distribution(graph,cumulative=F,mode="all")
  pdf(paste(outGraphDistDirPath,"/",plotFileName,"_degree_distribution_plot",".pdf",sep = ""))
  plot(dg[dg!=0], log="xy",main=paste("degree distribution for ",plotFileName),xlab="k", ylab="P(k)")
  #dev.copy(jpeg,filename=paste(outGraphDistDirPath,"/",plotFileName,"_degree_distribution_plot",".jpeg",sep = ""))
  dev.off ()
}