######Note: Must load pscl ahead of time for this to work. 
##Load the packages
library(pscl)
library(igraph)

#########IMPORTANT NOTE!!!!!!!!!!!!!!!!!!!!!! There are two different presidents listed 
#########for congress #27, so you will need to get rid of both!


##can currently only accept one congress for one chamber at a time

score.generator<-function(congress=1, abstain.agree=TRUE, senate=FALSE, lopn=.025){
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
  h.1<-h.1[-1,] ##Dropping President
  if(congress==27){h.1<-h.1[-1,]} ##Dropping president again for this Congress
  h.1[h.1==0]<-NA ##Converting 0s to NAs
  h.1<-na.omit(h.1) ##Omitting 0s aka Not in Legislature for one or more votes
  
  legnames<-rownames(h.1)
  thenames<-rownames(h.1)
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
  ##create two of these since I manipulate them differently
  a<-agree.mat(h.1)
  b<-agree.mat(h.1)
  
  #Make enough names for this process to work
  n<-length(rownames(a))
  enough.names<-rep(as.numeric(rownames(a)),n)
  repped.names<-rep(as.numeric(rownames(a)),c(rep(n,n)))
  
  #Set agreement to self to NA
  diag(a)<-NA
  
  ##Have a vector of these scores
  vector<-unlist(a)

  ##Remove agreement with self
  vector1<-na.omit(vector)
  
  ##Get rid of weird na.action attribute
  attr(vector1, "na.action")<-NULL
  
  ag.scores<-cbind(agreementscores=as.numeric(vector), icpsr1=as.numeric(enough.names), 
                   icpsr2=as.numeric(repped.names), congress=as.numeric(rep(congress, length(vector))))
  
  ag.scores<-na.omit(ag.scores) ##remove agreement with self  
  rownames(ag.scores)<-NULL
  
  ##Get the adjacency graph for centrality
  G <- graph.adjacency(as.matrix(b),weighted=TRUE)
  
  ## And then this will calculate and display the centrality scores
  centrality<-alpha.centrality(G)

  return(list(agreementscores=ag.scores, centrality=centrality))
}

score.generator(congress=1, abstain.agree=TRUE, senate=TRUE, lopn=.025)

##Try out the function
trial2<-score.generator(congress=1,abstain.agree=FALSE, senate=TRUE)
trial3<-score.generator(congress=1,abstain.agree=FALSE, senate=FALSE)
trial4<-score.generator(congress=1, abstain.agree=TRUE, senate=TRUE)
trial5<-score.generator(congress=1, abstain.agree=TRUE, senate=FALSE)

agree<-trial3$agreement.scores
agree2<-trial5$agreement.scores


plot(density(agree), col="red") ##any abstention auto treated as disagreement 
lines(density(agree2), col="blue") ##two abstains treated as agreements

agree3<-score.generator(20, FALSE, FALSE)$agreement.scores
agree4<-score.generator(20, TRUE, FALSE)$agreement.scores

plot(density(agree4), col="blue") ##two abstentions=agree
lines(density(agree3), col="red") ##abstentions auto disagree

#a.h1 <- as.data.frame(pairdata_full)
#colnames(a.h1) <- c("rc_agree")
#library(foreign)
#write.dta(a.h1,"agreefirsthouse.dta")
## Export the dataset
#write.dta(as.data.frame(centrality.h1),"centrality_h1.dta")