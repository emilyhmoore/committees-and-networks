######Note: Must load pscl ahead of time for this to work. 
##Load the packages
library(pscl)
library(igraph)
##can currently only accept one congress for one chamber at a time
##Will increase functionality to include option for house or senate and
##the ability to run multiple at the same time. 

score.generator<-function(congress=1, abstain.agree=TRUE){
  ##Congress needs to be altered to have a 0 in front if less than 10
  if (congress<10){congress<-paste("0",congress, sep="")
  } else {congress<-as.character(congress)}
  file<-paste("ftp://voteview.com/dtaord/hou",congress, "kh.ord", sep="")
  
  house<-readKH(file) ##read in the file
  n<-.025*house$n ##gets n for droping very lopsided votes
  house<-dropUnanimous(house,lop=n)
  h.1<-house$votes ##First Congress Roll Call Votes
  h.1<-h.1[-1,] ##Dropping President
  h.1[h.1==0]<-NA ##Converting 0s to NAs
  h.1<-na.omit(h.1) ##Omitting 0s aka Not in Legislature for one or more votes
  legnames<-rownames(h.1)
  agree.mat <- function(X, abstain=abstain.agree){
    X <- t(X) # put subjects on columns
    n <- ncol(X)
    A <- matrix(NA, n, n)
    if(abstain==TRUE){
      for (i in 1:n){
        A[i,] <- apply(X[,i] == X, 2, sum)
      }
    }else{
      for (i in 1:n){
        A[i,] <- apply(X[,i] == X & X!=9, 2, sum)
      }
    }
    rownames(A)<-legnames
    colnames(A)<-legnames
    A <- A/ncol(h.1)
    data <- as.data.frame(A)
    return(data)
  }
  a<-agree.mat(h.1)
  b<-agree.mat(h.1)
  legnames<-rownames(a)
  legnames1<-unlist(strsplit(legnames,split=" \\("))
  odds<-seq(1,length(legnames1), by=2)
  just.names<-legnames1[odds]
  n<-length(just.names)
  enough.names<-rep(just.names,n)
  repped.names<-rep(just.names,c(rep(n,n)))
  diag(a)<-NA
  vector<-unlist(a)
  for(i in 1:length(vector)){
    names(vector)[i]<-paste(repped.names[i],enough.names[i],sep="_")
  }
  vector<-na.omit(vector)
  G <- graph.adjacency(as.matrix(b),weighted=TRUE)
  ## And then this will calculate and display the centrality scores
  centrality<-alpha.centrality(G)
  #plot(density.default(x=alpha.centrality(G)))
  return(list("agreement.scores"=vector, "centrality.scores"=centrality))
}

trial2<-score.generator(congress=10,abstain.agree=FALSE)
trial<-score.generator(congress=10, abstain.agree=TRUE)
head(trial2$agreement.scores)
head(trial$agreement.scores)

#a.h1 <- as.data.frame(pairdata_full)
#colnames(a.h1) <- c("rc_agree")
#library(foreign)
#write.dta(a.h1,"agreefirsthouse.dta")
## Export the dataset
#write.dta(as.data.frame(centrality.h1),"centrality_h1.dta")