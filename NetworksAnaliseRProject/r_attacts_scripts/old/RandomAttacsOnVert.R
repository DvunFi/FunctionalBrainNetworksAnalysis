library(igraph)
#data frame for data of attack
i<-5
tableIndex<-0
prefix<-paste("m5","c0_RND_q=0",sep = "")
path<-paste("D:/VKR/VKRdocs/scripts/data/1000/Results/targetAttacsVer/For1000/m=5/RND/c4/",sep = "")
count_of_deleted_V<-0
stringname<-paste("D:/VKR/VKRdocs/scripts/data/1000/Environment1000m",i,".RData",sep="")
g<-readRDS(stringname)
#g <- readRDS("D:/VKR/VKRdocs/scripts/data/1000/1000_with_com_m5/c4/Environment1000m5c4-0.7428.RData")
Mean_K<-0
Max_connectivity_element<-0
count_of_deleted_V<-0
Modularity<-0
CommunitiesCount<-0
delet<-10
deleted<-0
max_conectivity_component<-0
mean_k<-0
modularity_louvain<-0
Total_E<-0
Total_V<-0
deleted_total<-0
V_bc<-betweenness(g, v = V(g), directed = FALSE)
max_BC<-max(V_bc)
min_BC<-min(V_bc)
data_frame_for_max_com_and_k<-data.frame(deleted,max_conectivity_component,mean_k,
                                         max_BC,min_BC,
                                         modularity_louvain,Total_V,Total_E,deleted_total)


  Max_connectivity_element<-max(sizes(clusters(g)))
  while (max(sizes(clusters(g)))/1000>0.05) {
    g<-delete.vertices(g,sample(V(g),delet))
    V_bc<-betweenness(g, v = V(g), directed = FALSE)
    fc2<-cluster_louvain(g)
    Max_connectivity_element<-max(sizes(clusters(g)))
    count_of_deleted_V<-count_of_deleted_V+delet
    data_frame_for_max_com_and_k[tableIndex,] = list(count_of_deleted_V,max(sizes(clusters(g)))/1000,
                                               mean(degree(g)),max(V_bc),min(V_bc),
                                               modularity(fc2),gorder(g),gsize(g),count_of_deleted_V/1000)
    tableIndex<-tableIndex+1
    stdname<-paste("График BC вершин"," удалено ",count_of_deleted_V," вершин", sep="")
    # hist(betweenness(g, v = V(g), directed = FALSE),main =stdname,breaks = 20,
    #      ylab="Количество вершин",xlab="Промежуточная значимость вершин")
    # dev.copy(jpeg,filename=paste(path,"/",stdname,"(случ V1000m5c0)",".jpg",sep = ""));
    # dev.off ();
  }
  plot(data_frame_for_max_com_and_k[[9]],data_frame_for_max_com_and_k[[2]] ,xlab="доля удаленных вершин", 
       ylab="ддоля вершин принадлежащих МСК", xlim=c(0,1),ylim = c(0,1))
  dev.copy(jpeg,filename=paste(path," Максимальная связаная элемента от доли удаленных верши(RND v1000",prefix,")",".jpg",sep = ""));
  dev.off ();
  stringname<-paste(path," Random_attacs_Table_Info_FOr_ 1000",prefix,".RData",sep="")
  saveRDS(data_frame_for_max_com_and_k,stringname)
  
  
  # 
  # plot(data_frame_for_max_com_and_k[[1]],data_frame_for_max_com_and_k[[2]],type ="o",xlab="удалено вершин", 
  #      ylab="отношение связаной элементы ко всем",main="Связаная элемента")
  # dev.copy(jpeg,filename=paste(path,"/отношение связаной элементы ко всем(случ V1000m5c0)",".jpg",sep = ""));
  # dev.off ();
  # plot(data_frame_for_max_com_and_k[[1]],data_frame_for_max_com_and_k[[3]],type ="o",xlab="удалено вершин", 
  #      ylab="средняя степень",main="Средняя степень вершин от кол-ва удаенных")
  # dev.copy(jpeg,filename=paste(path,"/Средняя степень вершин от кол-ва удаенных(случ V1000m5c0)",".jpg",sep = ""));
  # dev.off ();
  # plot(data_frame_for_max_com_and_k[[1]],data_frame_for_max_com_and_k[[6]],type ="o",xlab="удалено вершин", 
  #      ylab="модульность louvain",main="модульность от удаления вершин louvain")
  # dev.copy(jpeg,filename=paste(path,"/модульность от удаления вершин louvain(случ V1000m5c0)",".jpg",sep = ""));
  # dev.off ();
  # plot(data_frame_for_max_com_and_k[[1]],data_frame_for_max_com_and_k[[8]],type ="o",xlab="удалено вершин", 
  #      ylab="количество связий",main="количество связий от удаления вершин")
  # dev.copy(jpeg,filename=paste(path,"/количество связий от удаления вершин(случ V1000m5c0)",".jpg",sep = ""));
  # dev.off ();  
  # stringname<-paste("D:/VKR/VKRdocs/scripts/data/1000/Results/RandomAttacsVeR/For1000/m5c0/Random_Table_Info_FOr_1000m",i,".RData",sep="")
  # saveRDS(data_frame_for_max_com_and_k,stringname)