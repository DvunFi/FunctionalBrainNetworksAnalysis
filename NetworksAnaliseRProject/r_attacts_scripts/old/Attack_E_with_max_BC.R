library(igraph)
i<-5
prefix<-paste("m5","c0_E_BC_q=0",sep = "")
path<-paste("D:/VKR/VKRdocs/scripts/data/1000/Results/targetAttacsVer/For1000/m=5/E_BC/c4/",sep = "")
delet_count<-10
stringname<-paste("D:/VKR/VKRdocs/scripts/data/1000/Environment1000m",i,".RData",sep="")
g<-readRDS(stringname)
#g <- readRDS("D:/VKR/VKRdocs/scripts/data/1000/1000_with_com_m5/c4/Environment1000m5c3.RData")
E_BC<-edge_betweenness(g, e = E(g), directed = FALSE, weights = NULL)
max_BC<-max(E_BC)
min_BC<-min(E_BC)
mean_BC<-mean(E_BC)
betwen_1<-0
betwen_2<-0
count<-0
deleted<-0
max_conectivity_component<-0
mean_k<-0
modularity_louvain<-0
Total_E<-0
Total_V<-0
deleted_total<-0
data_frame_for_max_com_and_k<-data.frame(deleted,max_conectivity_component,mean_k,
                                         max_BC,min_BC,
                                         modularity_louvain,Total_V,Total_E,deleted_total)
step<-0
count_e<-length(E(g))
while(max(sizes(clusters(g))) / 1000 > 0.05) {
  id <- seq(E(g))
  df2 <- data.frame(E_BC, id)
  df2 <- df2[with(df2, order(-E_BC, id)),]
  vert <- df2[[2]]
  g <- delete.edges(g, vert[1:delet_count])
  E_BC <-
    edge_betweenness(g,
                     e = E(g),
                     directed = FALSE,
                     weights = NULL)
  max_BC <- max(E_BC)
  min_BC <- min(E_BC)
  deleted <- deleted + delet_count
  fc2 <- cluster_louvain(g)
  step <- step + 1
  data_frame_for_max_com_and_k[step, ] = list(
    deleted,
    max(sizes(clusters(g))) / 1000,
    mean(degree(g)),
    max_BC,
    min_BC,
    modularity(fc2),
    gorder(g),
    gsize(g),
    deleted / count_e
  )
}

plot(data_frame_for_max_com_and_k[[9]],data_frame_for_max_com_and_k[[2]] ,xlab="доля удаленных связей", 
     ylab="доля вершин принадлежащих МСК", xlim=c(0,1),ylim = c(0,1))
dev.copy(jpeg,filename=paste(path,"Максимальная связаная элемента от доли удаленных связей(направ v1000",prefix,")",".jpg",sep = ""));
dev.off ();
stringname<-paste(path,"Target_BC_E_Table_Info_FOr_ 1000",prefix,i,".RData",sep="")
saveRDS(data_frame_for_max_com_and_k,stringname)
