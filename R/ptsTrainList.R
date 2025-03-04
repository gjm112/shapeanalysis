ptsTrainList<-list()
refFile<-data.frame()

setwd("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/Point files/")

tribeVec <- list.files()
for (t in tribeVec){print(t)
  speciesVec<-list.files(paste0("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/Point files/",t))
  for (s in speciesVec){print(s)
    teethVec<-list.files(paste0("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/Point files/",t,"/",s))
    for (tooth in teethVec){
      filesVec<-list.files(paste0("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/Point files/",t,"/",s,"/",tooth))
      for (f in filesVec){
        file<-(paste0("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/Point files/",t,"/",s,"/",tooth,"/",f))
        
        if (file != "/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/Point files/Antilopini/Antidorcas marsupialis/LM2 A marsupialis/DSCN3190"){
          temp<-matrix(unlist(read.table(file))[-c(1:20)],ncol=13,byrow=TRUE)[,-13]
          
          pts<-data.frame(x=c(t(temp[,seq(1,11,2)])),y=c(t(temp[,seq(1,11,2)+1])))
        }
        
        if (file == "/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/Point files/Antilopini/Antidorcas marsupialis/LM2 A marsupialis/DSCN3190"){
          pts<-read.table(file)
          names(pts)<-c("x","y")
        }
        
        #Check if the tooth is in the test data set.  If it is don't add it to the training data set.
        if (!f%in%toupper(names(imgsListMean))){
          ptsTrainList[[substring(tooth,1,3)]][[f]]<-as.matrix(pts)
          refFile<-rbind(refFile,data.frame(ref=f,tooth=substring(tooth,1,3),tribe=t,species=s))
        }
        
      }
    }
  }
}
