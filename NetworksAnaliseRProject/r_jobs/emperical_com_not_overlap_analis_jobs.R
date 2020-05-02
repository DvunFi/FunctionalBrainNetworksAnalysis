source("general_funtions.R")
#library(parallel)

inputDirPath <- "../input/adjacencyMatrices/"
baseOtputDir <- "../output/"

datePostfix <- gsub(" ","_",gsub(":","_",gsub("-","_",Sys.time())))

outDirPath <- paste(baseOtputDir, "out_", datePostfix, sep = "")
dir.create(outDirPath)

listOfInputsDirsNames <- list.dirs("input/adjacencyMatrices/", full.names = FALSE)

for(subjectName in listOfInputsDirsNames){
  if(subjectName!=""){
    listOfCSVFiles <-list.files(paste("input/adjacencyMatrices/",subjectName, sep=""))
    job_name <- paste(subjectName,"_def_com_structure_job",sep="")
    rstudioapi::jobRunScript(path = "./r_networks_analitics_scripts/emperical_networks_non_overlapp_com_analis.R",name=paste(datePostfix,job_name,sep=""), importEnv = TRUE)
  }
}