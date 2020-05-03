library(igraph)
delet_count<-10
stringname<-paste("D:/VKR/VKRdocs/scripts/data/1000/Environment1000m",5,".RData",sep="")
g<-readRDS(stringname)
V_bc<-betweenness(g, v = V(g), directed = FALSE)
max_BC<-max(V_bc)
min_BC<-min(V_bc)
mean_BC<-mean(V_bc)
k<-round(1+3.322*log10(length(V_bc)), digits = 0)+2
h<-round((max_BC-min_BC)/k,digits = 0)
betwen_1<-0
betwen_2<-0
count<-0
data_frame_of_Dist_BC<-data.frame(betwen_1,betwen_2,count)
for(i in 1:k){
  kc<-c(V_bc[V_bc>(i-1)*(h) & V_bc<(i)*(h)])
  data_frame_of_Dist_BC[i,] = list((i-1)*(h),(i)*(h),length(kc))
}
plot(data_frame_of_Dist_BC[[1]],data_frame_of_Dist_BC[[3]],type ="s",xlab="Промежуточная значимость вершин", 
     ylab="Количество вершин",main = "График промежуточной значимости вершин без удалений",
     ylim=c(0,max(data_frame_of_Dist_BC[[3]])),xlim=c(0,max(data_frame_of_Dist_BC[[1]])))
dev.copy(jpeg,filename=paste("D:/VKR/VKRdocs/scripts/data/","График промежуточной значимости вершин без удалений",".jpg",sep = ""));
dev.off ();
deleted<-0
max_conectivity_component<-0
mean_k<-0
data_frame_for_max_com_and_k<-data.frame(deleted,max_conectivity_component,mean_k,max_BC,min_BC)
step<-0
while(max(sizes(clusters(g)))/1000>0.05){
  id<-seq(V(g))
  df2<-data.frame(V_bc,id)
  df2<-df2[with(df2, order(-V_bc,id)), ]
  vert<-df2[[2]]
  g<-delete.vertices(g,vert[1:delet_count])
  V_bc<-betweenness(g, v = V(g), directed = FALSE)
  gorder(g)
  max_BC<-max(V_bc)
  min_BC<-min(V_bc)
  k<-round(1+3.322*log10(length(V_bc)), digits = 0)
  h<-round((max_BC-min_BC)/k,digits = 0)
  # data_frame_of_Dist_BC<-data.frame(betwen_1,betwen_2,count)
  # for(i in 1:k){
  #   kc<-c(V_bc[V_bc>(i-1)*(h) & V_bc<=(i)*(h)])
  #   data_frame_of_Dist_BC[i,] = list((i-1)*(h),(i)*(h),length(kc))
  # }
  deleted<-deleted+delet_count
  #максимальная связная элемента отношение к ощему количеству вершин
  step<-step+1
  data_frame_for_max_com_and_k[step,] = list(deleted,max(sizes(clusters(g)))/1000,mean(degree(g)),max(V_bc),min(V_bc))
   stdname<-paste("График BC вершин"," удалено ",deleted," вершин", sep="")
   # plot(data_frame_of_Dist_BC[[1]],data_frame_of_Dist_BC[[3]],type ="s",xlab="Промежуточная значимость вершин", 
   #      ylab="Количество вершин",main =stdname,
   #      ylim=c(0,max(data_frame_of_Dist_BC[[3]])),xlim=c(0,max(data_frame_of_Dist_BC[[1]])))
  hist(betweenness(g, v = V(g), directed = FALSE),main =stdname,breaks = 20,
       xlab="Количество вершин",ylab="Промежуточная значимость вершин")
  dev.copy(jpeg,filename=paste("D:/VKR/VKRdocs/scripts/data/",stdname,".jpg",sep = ""));
  dev.off ();
}


plot(data_frame_for_max_com_and_k[[1]],data_frame_for_max_com_and_k[[2]],type ="o",xlab="удалено вершин", 
     ylab="отношение связаной элементы ко всем",main="Связаная элемента")
dev.copy(jpeg,filename=paste("D:/VKR/VKRdocs/scripts/data/","отношение связаной элементы ко всем",".jpg",sep = ""));
dev.off ();
plot(data_frame_for_max_com_and_k[[1]],data_frame_for_max_com_and_k[[3]],type ="o",xlab="удалено вершин", 
     ylab="средняя степень",main="Средняя степень вершин от кол-ва удаенных")
dev.copy(jpeg,filename=paste("D:/VKR/VKRdocs/scripts/data/","Средняя степень вершин от кол-ва удаенных",".jpg",sep = ""));
dev.off ();


