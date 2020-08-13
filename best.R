best<-function(state,outcome){
     
     setwd("C:/Users/hp/Desktop/R/PA3/rprog_data_ProgAssignment3-data")
     oc<-read.csv("outcome-of-care-measures.csv",colClasses="character")
     setwd("C:/Users/hp/Desktop/R/PA3")
     if(sum(oc$State==state)==0){
          stop("invalid state")
     }
     oc<-oc[oc$State==state,]
     find<-which(c("heart attack","heart failure","pneumonia")==outcome,arr.ind=TRUE)*6+5
     if(length(find)==0){
          stop("invalid outcome")
     }
     oc<-data.frame(Hospital_Name=oc[,2],out=oc[,find])
     oc<-oc[!oc$out=="Not Available",]
     oc<-oc[as.numeric(oc$out)==min(as.numeric(oc$out)),]
     min(oc$Hospital_Name)
          
     
}