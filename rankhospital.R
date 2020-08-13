rankhospital<-function(state,outcome,num){
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
        oc <-oc[order(as.numeric(oc[,2]),oc$Hospital.Name),]
        
        
        if(num=="best"){num<-1}
        if(num=="worst"){num<-nrow(oc)}
        if(num>nrow(oc)){NA}
        else{oc[num,1]}
        
     
}