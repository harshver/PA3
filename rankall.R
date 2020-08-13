rankall<-function(outcome,num){
     setwd("C:/Users/hp/Desktop/R/PA3/rprog_data_ProgAssignment3-data")
     oc<-read.csv("outcome-of-care-measures.csv",colClasses="character")
     setwd("C:/Users/hp/Desktop/R/PA3")
     states<-unique(oc$State)
     states<-states[order(states)]
     ans<-data.frame(Hospital.Name=rep(NA,length(states)),State=states)
     find<-which(c("heart attack","heart failure","pneumonia")==outcome,arr.ind=TRUE)*6+5
     if(length(find)==0){
          stop("invalid outcome")
     }
     oc<-oc[c(2,7,find)]
     oc<-oc[!oc[,3]=="Not Available",]
     oc <-oc[order(oc$State),]
     
     for(i in 1:length(states)){
          new_oc<-oc[oc[,2]==states[i],]
          new_oc <- new_oc[order(as.numeric(new_oc[,3]),new_oc[,1]),]
          if(num=="best"){num<-1}
          if(num=="worst"){num<-nrow(new_oc)}
          if(num<=nrow(new_oc)){ans[i,1]<-new_oc[num,1]}
     }
     ans
}