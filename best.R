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
     oc<-oc[c(2,find)]
     oc<-oc[!oc[,2]=="Not Available",]
     oc<-oc[as.numeric(oc[,2])==min(as.numeric(oc[,2])),]
     min(oc[,1])
          
     
}