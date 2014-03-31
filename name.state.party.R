##Takes a returned name vector from the previous function and returns a matrix of names and states
library(plyr)

cong.names<-function(i){
  score.generator(congress=i, TRUE, senate=FALSE)$names}

  congress.names<-llply(1:27, cong.names)
congress.names

congress.names[[1]][48]<-"MUHLENGBRG (NA PA-98)"
congress.names[[6]][6]<-"GOODRICH (Federalist CT-98)"
congress.names[[22]]<-gsub(pattern="Anti-Masonic", replacement="AntiMasonic",congress.names[[22]])
congress.names[[23]]<-gsub(pattern="Anti Masonic", replacement="AntiMasonic",congress.names[[23]])
congress.names[[24]]<-gsub(pattern="Anti Masonic", replacement="AntiMasonic",congress.names[[24]])

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
name.state(congress.names[[24]])
