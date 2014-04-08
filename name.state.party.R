##Takes a returned name vector from the previous function and returns a matrix of names and states
library(plyr)
library(pscl)
cong1<-(readKH("ftp://voteview.com/dtaord/hou01kh.ord"))
cong1$legis.data

names.states1<-function(congress=1, senate){
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
  h.1<-house$legis.data ##Roll Call Votes
  h.1<-h.1[-1,] ##Dropping President
  if(congress==27){h.1<-h.1[-1,]}
  return(h.1)}

##this creates a list of all the congresses
cong<-llply(1:27, names.states1, senate=FALSE)

##This makes each state a character instead of a factor
for (i in 1:27){cong[[i]]$state<-as.character(cong[[i]]$state)
}

setwd("C:/Users/emily m/Journal Articles/Committees and Networks")
library(foreign)
yr1790<-read.dta("1790.dta")
popdata.1790<-yr1790[yr1790$level==2,]

yr1800<-read.dta("1800.dta")
popdata.1800<-yr1800[yr1800$level==2,]

yr1810<-read.dta("1810.dta")
popdata.1810<-yr1810[yr1810$level==2,]

yr1820<-read.dta("1820.dta")
popdata.1820<-yr1820[yr1820$level==2,]

yr1830<-read.dta("1830.dta")
popdata.1830<-yr1830[yr1830$level==2,]

yr1840<-read.dta("1840.dta")
popdata.1840<-yr1840[yr1840$level==2,]

##Takes R's built in state information
states<-cbind(name=toupper(state.name),state.abb,state.division=as.character(state.division))

##This is the state data matched with the population data.
d1790<-merge(states, popdata.1790, stringsAsFactors=FALSE)

d1800<-merge(states, popdata.1800, stringsAsFactors=FALSE)

d1810<-merge(states, popdata.1810, stringsAsFactors=FALSE)

d1820<-merge(states, popdata.1820, stringsAsFactors=FALSE)

d1830<-merge(states, popdata.1830, stringsAsFactors=FALSE)

d1840<-merge(states, popdata.1840, stringsAsFactors=FALSE)

##Changing the column names so they're unique. 
colnames(d1840)[c(1,2,4)]<-
  colnames(d1830)[c(1,2,4)]<-
  colnames(d1820)[c(1,2,4)]<-
  colnames(d1810)[c(1,2,4)]<-
  colnames(d1800)[c(1,2,4)]<-
  colnames(d1790)[c(1,2,4)]<-c("fullstatename", "state", "statecode")


##telling the first congress list to contain the list of members as a column not just row names.
cong[[1]]$members<-rownames(cong[[1]])

cong[[6]]$members<-rownames(cong[[6]])

cong[[11]]$members<-rownames(cong[[11]])

cong[[16]]$members<-rownames(cong[[16]])

cong[[21]]$members<-rownames(cong[[21]])

cong[[26]]$members<-rownames(cong[[26]])

##merging the state pop data to the congress member data
firsthouse<-merge(cong[[1]], d1790, by="state")

sixthhouse<-merge(cong[[6]], d1800, by="state")

eleventhhouse<-merge(cong[[11]], d1810, by="state")

sixteenthhouse<-merge(cong[[16]], d1820)

twfirsthouse<-merge(cong[[21]], d1830)

twsixthhouse<-merge(cong[[26]], d1840)

##delete some useless columns
firsthouse<-(firsthouse[-c(8:11)])
sixthhouse<-sixthhouse[-c(8)]
eleventhhouse<-eleventhhouse[-c(8)]
sixteenthhouse<-sixteenthhouse[-c(8)]
twfirsthouse<-twfirsthouse[-c(8)]
twsixthhouse<-twsixthhouse[-c(8)]

##make the members names the first column. 
firsthouse<-cbind(firsthouse$members, firsthouse[,-7])
sixthhouse<-cbind(sixthhouse$members, sixthhouse[,-7])
eleventhhouse<-cbind(eleventhhouse$members, eleventhhouse[,-7])
sixteenthhouse<-cbind(sixteenthhouse$members, sixteenthhouse[,-7])
twfirsthouse<-cbind(twfirsthouse$members, twfirsthouse[,-7])
twsixthhouse<-cbind(twsixthhouse$members, twsixthhouse[,-7])


##OUtdated. May need later, so keeping. Use above code. 
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

##Stewart Data stuff
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
