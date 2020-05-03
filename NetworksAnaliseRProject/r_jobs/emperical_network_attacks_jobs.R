  inputDirPath <- "../input/empericalDataSource/sourceForAttacs/"
  baseOtputDir <- "output/networksAttacks/empericalNetworks/"
  datePostfix <- gsub(" ","_",gsub(":","_",gsub("-","_",Sys.time())))
  
  outDirPath <- paste( baseOtputDir, "out_attacks_", datePostfix, sep = "")
  dir.create(outDirPath, recursive = TRUE)
  outDirPath <- paste("../",outDirPath,sep = "")
    
  listOfInputsDirsNames <- list.dirs("input/empericalDataSource/sourceForAttacs", full.names = FALSE)
  
  for(subjectName in listOfInputsDirsNames){
    if(subjectName!=""){
      listOfCSVFiles <-list.files(paste("input/empericalDataSource/sourceForAttacs/",subjectName, sep=""))
      #rstudioapi::jobRunScript(path = "r_attacts_scripts/emperical_networks_random_attacks_run.R",name=paste("emperical_networks_random_attacks ", datePostfix,sep=""), importEnv = TRUE)
      rstudioapi::jobRunScript(path = "r_attacts_scripts/emperical_networks_target_attacks_v_max_bc_run.R",name=paste("emperical_networks_target_attacks_v_max_bc ", datePostfix,sep=""), importEnv = TRUE)
      rstudioapi::jobRunScript(path = "r_attacts_scripts/emperical_networks_target_attacks_e_max_bc_run.R",name=paste("emperical_networks_target_attacks_e_max_bc ", datePostfix,sep=""), importEnv = TRUE)
      #rstudioapi::jobRunScript(path = "r_attacts_scripts/emperical_networks_target_attacks_max_degree_run.R",name=paste("emperical_networks_attacks_max_degree " , datePostfix,sep=""), importEnv = TRUE)
   }
  }