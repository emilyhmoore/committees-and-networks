##Takes a returned name vector from the previous function and returns a matrix of names and states
library(plyr)

cong.names<-function(i){
  score.generator(congress=i, TRUE, senate=FALSE)$names}

  congress.names<-llply(1:27, cong.names)
congress.names
cong.names(27)
congress.names[[1]][48]<-"MUHLENGBRG (NA PA-98)"
congress.names[[6]][6]<-"GOODRICH (Federalist CT-98)"
congress.names[[22]]<-gsub(pattern="Anti-Masonic", replacement="AntiMasonic",congress.names[[22]])
congress.names[[23]]<-gsub(pattern="Anti Masonic", replacement="AntiMasonic",congress.names[[23]])
congress.names[[24]]<-gsub(pattern="Anti Masonic", replacement="AntiMasonic",congress.names[[24]])
congress.names[[25]]<-gsub(pattern="Anti Masonic", replacement="AntiMasonic",congress.names[[25]])
congress.names[[26]]<-gsub(pattern="Anti Masonic", replacement="AntiMasonic",congress.names[[26]])
congress.names[[27]]<-cong.names(27)
congress.names[[27]][19]<-"CASEY (Ind.Democrat IL-2)"

name.state<-function(x){
  
  cong1.state<-unlist(strsplit(x, " "))
  
  cong1.state<-unlist(strsplit(cong1.state, "-"))
  
  by.two<-seq(1, length(cong1.state), by=2)
  
  list<-cong1.state[by.two]
  
  data<-matrix(list, ncol=2, byrow=TRUE)
  colnames(data)<-c("names", "state")
  return(data)
}
##1-5 are fine. Will need to fix 6, 7-21 is fine. 22 needs work.
cong.names.states<-llply(congress.names, name.state)
cong.names.states
setwd("C:/Users/emily m/Journal Articles/Committees and Networks")
library(foreign)
stewart.data<-read.dta("Stewart House committees.dta")
index<-which(stewart.data$cong<28)
stewart.data<-stewart.data[index,]
stewart.data$partystring<-stewart.data$party

stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==5000), 
                                  "Pro-Administration")

stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==200), 
                                  "Republican")

stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==100), 
                                  "Democrat")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==4000), 
                                  "Anti-Administration")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==1), 
                                  "Federalist")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==7777), 
                                  "Crawford Republican")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==8888), 
                                  "Adams-Clay Republican")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==1346), 
                                  "Jackson Republican")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==8000), 
                                  "Adams-Clay Federalist")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==7000), 
                                  "Jackson Federalist")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==6000), 
                                  "Crawford Federalist ")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==555), 
                                  "Jackson")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==22), 
                                  "Adams")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==1275), 
                                  "Anti-Jackson")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==26), 
                                  "Anti-Masonic")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==44), 
                                  "Nullifier")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==29), 
                                  "Whig")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==104), 
                                  "Van Buren Democrat")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==328), 
                                  "Independent")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==112), 
                                  "Conservative")
stewart.data$partystring<-replace(stewart.data$partystring, list=which(stewart.data$partystring==329), 
                                  "Ind. Democrat")

lastnames<-unlist(strsplit(stewart.data$name, split=","))[seq(1,(6293*2), by=2)]

stewart.data$lastnames<-lastnames
head(stewart.data)

seps<-function(i){
  houses<-list()
  length(houses)<-1
  names<-paste("stew", 1:27, sep="")
  names(houses)<-names[i]
  houses<-stewart.data[which(stewart.data$cong==i),]
  return(houses)
}
trial<-llply(1:27, seps)
trial[[27]]
