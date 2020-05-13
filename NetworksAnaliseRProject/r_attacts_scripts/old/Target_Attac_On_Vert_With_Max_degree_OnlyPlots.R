library(igraph)
i<-5
prefix<-paste("m",i,"c0_k_q=0",sep = "")
path<-paste("D:/VKR/VKRdocs/scripts/data/1000/Results/targetAttacsVer/For1000/m=5/K/c4/",sep = "")
delet_count<-10
stringname<-paste("D:/VKR/VKRdocs/scripts/data/1000/Environment1000m",i,".RData",sep="")
g<-readRDS(stringname)
#g <- readRDS("D:/VKR/VKRdocs/scripts/data/1000/1000_with_com_m5/c4/Environment1000m5c4-0.7428.RData")
V_max_ks<-degree(g)
max_BC<-max(betweenness(g, v = V(g), directed = FALSE))
min_BC<-min(betweenness(g, v = V(g), directed = FALSE))
betwen_1<-0
betwen_2<-0
count<-0
# hist(betweenness(g, v = V(g), directed = FALSE),main ="√рафик промежуточной значимости вершин без удалений",breaks = 20,
#      ylab=" оличество вершин",xlab="ѕромежуточна€ значимость вершин")
# dev.copy(jpeg,filename=paste(path,"/√рафик промежуточной значимости вершин без удалений",".jpg",sep = ""));
# dev.off ();
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
while(max(sizes(clusters(g)))/1000>0.005){
  id<-seq(V(g))
  df2<-data.frame(V_max_ks,id)
  df2<-df2[with(df2, order(-V_max_ks,id)), ]
  vert<-df2[[2]]
  g<-delete.vertices(g,vert[1:delet_count])
  V_max_ks<-degree(g)
  V_bc<-betweenness(g, v = V(g), directed = FALSE)
  max_BC<-max(betweenness(g, v = V(g), directed = FALSE))
  min_BC<-min(betweenness(g, v = V(g), directed = FALSE))
  deleted<-deleted+delet_count
  fc2<-cluster_louvain(g)
  #максимальна€ св€зна€ элемента отношение к ощему количеству вершин
  step<-step+1
  data_frame_for_max_com_and_k[step,] = list(deleted,max(sizes(clusters(g)))/1000,
                                             mean(degree(g)),max(V_bc),min(V_bc),
                                             modularity(fc2),gorder(g),gsize(g),deleted/1000)
  # stdname<-paste("√рафик BC вершин,"," удалено ",deleted," вершин (направ v1000",prefix,")", sep="")
  # hist(betweenness(g, v = V(g), directed = FALSE),main =stdname,breaks = 20,
  #      ylab=" оличество вершин",xlab="промежуточна€ значимость")
  # dev.copy(jpeg,filename=paste(path,"/",stdname,".jpg",sep = ""));
  # dev.off ();
  
  # stdname<-paste("√рафик степеней вершин,"," удалено ",deleted," вершин (направ v1000",prefix,")", sep="")
  # hist(degree(g),main =stdname,breaks = 20,
  #      ylab=" оличество вершин",xlab="степени вершин")
  # dev.copy(jpeg,filename=paste(path,"/",stdname,".jpg",sep = ""));
  # dev.off ();
}


plot(data_frame_for_max_com_and_k[[9]],data_frame_for_max_com_and_k[[2]],xlab="удалено вершин", 
     ylab="дол€ вершин принадлежащих ћ— ", xlim=c(0,0.5),ylim = c(0,1))
dev.copy(jpeg,filename=paste(path,"/отношение св€заной элементы ко всем(направ v1000",prefix,")",".jpg",sep = ""));
dev.off ();
# plot(data_frame_for_max_com_and_k[[9]],data_frame_for_max_com_and_k[[3]],xlab="длол€ удаленых вершин", 
#      ylab="средн€€ степень",main="—редн€€ степень вершин от кол-ва удаенных")
# dev.copy(jpeg,filename=paste(path,"/—редн€€ степень вершин от кол-ва удаенных(направ v1000",prefix,")",".jpg",sep = ""));
# dev.off ();
# plot(data_frame_for_max_com_and_k[[1]],data_frame_for_max_com_and_k[[6]],xlab="удалено вершин", 
#      ylab="модульность louvain",main="модульность от удалени€ вершин louvain")
# dev.copy(jpeg,filename=paste(path,"/модульность от удалени€ вершин louvain(направ v1000",prefix,")",".jpg",sep = ""));
# dev.off ();
# plot(data_frame_for_max_com_and_k[[9]],data_frame_for_max_com_and_k[[8]],xlab="дол€ удаленых вершин", 
#      ylab="количество св€зий",main="количество св€зий от доли удаленных вершин")
# dev.copy(jpeg,filename=paste(path,"/количество св€зий от удалени€ вершин(направ v1000",prefix,")",".jpg",sep = ""));
# dev.off ();
stringname<-paste(path,"/Target_K_Table_Info_FOr_1000",prefix,".RData",sep="")
saveRDS(data_frame_for_max_com_and_k,stringname)

