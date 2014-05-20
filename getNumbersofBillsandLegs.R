num.bill.leg<-function(congress=1, senate=TRUE, lopn=.025){
  ##Congress needs to be altered to have a 0 in front if less than 10
  if (congress<10){congress<-paste("0",congress, sep="")
  } else {congress<-as.character(congress)}
  
  ##Path deciding whether to load up senate or house
  if(senate==FALSE){
    file<-paste("ftp://voteview.com/dtaord/hou",congress, "kh.ord", sep="")
  } else {
    file<-paste("ftp://voteview.com/dtaord/sen",congress, "kh.ord", sep="")
  }
  
  house<-readKH(file) ##read in the file
  n<-lopn*house$n ##gets n for dropping very lopsided votes
  icpsr<-(house$legis.data$icpsrLegis)
  rownames(house$votes)<-icpsr
  house<-dropUnanimous(house,lop=n)
  h.1<-house$votes ##Roll Call Votes
  leg<-nrow(h.1)
  bill<-ncol(h.1)
  return(c(leg, bill))
}