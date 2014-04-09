##Takes a returned name vector from the previous function and returns a matrix of names and states
library(plyr)
library(pscl)

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
sen<-llply(1:27, names.states1, senate=TRUE)

##This makes each state a character instead of a factor
for (i in 1:27){cong[[i]]$state<-as.character(cong[[i]]$state)}
for (i in 1:27){sen[[i]]$state<-as.character(sen[[i]]$state)}

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


##telling the congress lists to contain the list of members as a column not just row names.

for (i in 1:27){
  cong[[i]]$members<-rownames(cong[[i]])
  sen[[i]]$members<-rownames(sen[[i]])
}

##merging the state pop data to the congress member data
house1<-merge(cong[[1]], d1790, by="state")
senate1<-merge(sen[[1]], d1790, by="state")
house2<-merge(cong[[2]], d1790, by="state")
senate2<-merge(sen[[2]], d1790, by="state")
house3<-merge(cong[[3]], d1790, by="state")
senate3<-merge(sen[[3]], d1790, by="state")
house4<-merge(cong[[4]], d1790, by="state")
senate4<-merge(sen[[4]], d1790, by="state")
house5<-merge(cong[[5]], d1790, by="state")
senate5<-merge(sen[[5]], d1790, by="state")
house6<-merge(cong[[6]], d1790, by="state")
senate6<-merge(sen[[6]], d1790, by="state")

house7<-merge(cong[[7]], d1800, by="state")
senate7<-merge(sen[[7]], d1800, by="state")
house8<-merge(cong[[8]], d1800, by="state")
senate8<-merge(sen[[8]], d1800, by="state")
house9<-merge(cong[[9]], d1800, by="state")
senate9<-merge(sen[[9]], d1800, by="state")
house10<-merge(cong[[10]], d1800, by="state")
senate10<-merge(sen[[10]], d1800, by="state")
house11<-merge(cong[[11]], d1800, by="state")
senate11<-merge(sen[[11]], d1800, by="state")


house12<-merge(cong[[12]], d1810, by="state")
senate12<-merge(sen[[12]], d1810, by="state")
house13<-merge(cong[[13]], d1810, by="state")
senate13<-merge(sen[[13]], d1810, by="state")
house14<-merge(cong[[14]], d1810, by="state")
senate14<-merge(sen[[14]], d1810, by="state")
house15<-merge(cong[[15]], d1810, by="state")
senate15<-merge(sen[[15]], d1810, by="state")
house16<-merge(cong[[16]], d1810, by="state")
senate16<-merge(sen[[16]], d1810, by="state")

house17<-merge(cong[[17]], d1820, by="state")
senate17<-merge(sen[[17]], d1820, by="state")
house18<-merge(cong[[18]], d1820, by="state")
senate18<-merge(sen[[18]], d1820, by="state")
house19<-merge(cong[[19]], d1820, by="state")
senate19<-merge(sen[[19]], d1820, by="state")
house20<-merge(cong[[20]], d1820, by="state")
senate20<-merge(sen[[20]], d1820, by="state")
house21<-merge(cong[[21]], d1820, by="state")
senate21<-merge(sen[[21]], d1820, by="state")


house22<-merge(cong[[22]], d1830)
senate22<-merge(sen[[22]], d1830)
house23<-merge(cong[[23]], d1830)
senate23<-merge(sen[[23]], d1830)
house24<-merge(cong[[24]], d1830)
senate24<-merge(sen[[24]], d1830)
house25<-merge(cong[[25]], d1830)
senate25<-merge(sen[[25]], d1830)
house26<-merge(cong[[26]], d1830)
senate26<-merge(sen[[26]], d1830)


house27<-merge(cong[[27]], d1840)
senate27<-merge(sen[[27]], d1840)

##make the members names the first column. 

namesfirst<-function(x){
  x<-cbind(x$members, x[,-7])
  colnames(x)[1]<-"members"
  return(x)
}

house1<-namesfirst(house1)
house2<-namesfirst(house2)
house3<-namesfirst(house3)
house4<-namesfirst(house4)
house5<-namesfirst(house5)
house6<-namesfirst(house6)
house7<-namesfirst(house7)
house8<-namesfirst(house8)
house9<-namesfirst(house9)
house10<-namesfirst(house10)
house11<-namesfirst(house11)
house12<-namesfirst(house12)
house13<-namesfirst(house13)
house14<-namesfirst(house14)
house15<-namesfirst(house15)
house16<-namesfirst(house16)
house17<-namesfirst(house17)
house18<-namesfirst(house18)
house19<-namesfirst(house19)
house20<-namesfirst(house20)
house21<-namesfirst(house21)
house22<-namesfirst(house22)
house23<-namesfirst(house23)
house24<-namesfirst(house24)
house25<-namesfirst(house25)
house26<-namesfirst(house26)
house27<-namesfirst(house27)

senate1<-namesfirst(senate1)
senate2<-namesfirst(senate2)
senate3<-namesfirst(senate3)
senate4<-namesfirst(senate4)
senate5<-namesfirst(senate5)
senate6<-namesfirst(senate6)
senate7<-namesfirst(senate7)
senate8<-namesfirst(senate8)
senate9<-namesfirst(senate9)
senate10<-namesfirst(senate10)
senate11<-namesfirst(senate11)
senate12<-namesfirst(senate12)
senate13<-namesfirst(senate13)
senate14<-namesfirst(senate14)
senate15<-namesfirst(senate15)
senate16<-namesfirst(senate16)
senate17<-namesfirst(senate17)
senate18<-namesfirst(senate18)
senate19<-namesfirst(senate19)
senate20<-namesfirst(senate20)
senate21<-namesfirst(senate21)
senate22<-namesfirst(senate22)
senate23<-namesfirst(senate23)
senate24<-namesfirst(senate24)
senate25<-namesfirst(senate25)
senate26<-namesfirst(senate26)
senate27<-namesfirst(senate27)

head(house1)

categories.1790<-c("members", 'state', 'icpsrState', 'icpsrLegis', 'party', 'partyCode', 
                   'state.division', 'totpop', 'urb790', 'urb25', 'ofptot', 'stot', 'wmtot', 
                   'fips', 'statefip', 'region1', 'region2')

categories.1800<-c("members", 'state', 'icpsrState', 'icpsrLegis', 'party', 'partyCode', 
                   'state.division', 'totpop', 'urb800', 'urb25', 'ofptot', 'stot', 'wmtot', 
                   'fips', 'statefip', 'region1', 'region2')

categories.1810<-c("members", 'state', 'icpsrState', 'icpsrLegis', 'party', 'partyCode', 
                   'state.division', 'totpop', 'urb810', 'urb25', 'ofptot', 'stot', 'wmtot', 
                   'fips', 'statefip', 'region1', 'region2')

categories.1820<-c("members", 'state', 'icpsrState', 'icpsrLegis', 'party', 'partyCode', 
                   'state.division', 'totpop', 'urb820', 'urb25', 'smtot', 'sftot', 'wmtot', 
                   'fips', 'statefip', 'region1', 'region2','agricul', 'manufact', 'commerce')

categories.1830<-c("members", 'state', 'icpsrState', 'icpsrLegis', 'party', 'partyCode', 
                   'state.division', 'totpop', 'urb830', 'urb25', 'stot', 'wmtot', 
                   'fips', 'statefip', 'region1', 'region2')

categories.1840<-c("members", 'state', 'icpsrState', 'icpsrLegis', 'party', 'partyCode', 
                   'state.division', 'totpop', 'urb840', 'urb25', 'stot', 'wmtot', 
                   'fips', 'statefip', 'region1', 'region2')

house1<-house1[,categories.1790]
house2<-house2[,categories.1790]
house3<-house3[,categories.1790]
house4<-house4[,categories.1790]
house5<-house5[,categories.1790]
house6<-house6[,categories.1790]

house7<-house7[,categories.1800]
house8<-house8[,categories.1800]
house9<-house9[,categories.1800]
house10<-house10[,categories.1800]
house11<-house11[,categories.1800]

house12<-house12[,categories.1810]
house13<-house13[,categories.1810]
house14<-house14[,categories.1810]
house15<-house15[,categories.1810]
house16<-house16[,categories.1810]

house17<-house17[,categories.1820]
house18<-house18[,categories.1820]
house19<-house19[,categories.1820]
house20<-house20[,categories.1820]
house21<-house21[,categories.1820]

house22<-house22[,categories.1830]
house23<-house23[,categories.1830]
house24<-house24[,categories.1830]
house25<-house25[,categories.1830]
house26<-house26[,categories.1830]

house27<-house27[,categories.1840]

senate1<-senate1[,categories.1790]
senate2<-senate2[,categories.1790]
senate3<-senate3[,categories.1790]
senate4<-senate4[,categories.1790]
senate5<-senate5[,categories.1790]
senate6<-senate6[,categories.1790]

senate7<-senate7[,categories.1800]
senate8<-senate8[,categories.1800]
senate9<-senate9[,categories.1800]
senate10<-senate10[,categories.1800]
senate11<-senate11[,categories.1800]

senate12<-senate12[,categories.1810]
senate13<-senate13[,categories.1810]
senate14<-senate14[,categories.1810]
senate15<-senate15[,categories.1810]
senate16<-senate16[,categories.1810]

senate17<-senate17[,categories.1820]
senate18<-senate18[,categories.1820]
senate19<-senate19[,categories.1820]
senate20<-senate20[,categories.1820]
senate21<-senate21[,categories.1820]

senate22<-senate22[,categories.1830]
senate23<-senate23[,categories.1830]
senate24<-senate24[,categories.1830]
senate25<-senate25[,categories.1830]
senate26<-senate26[,categories.1830]

senate27<-senate27[,categories.1840]

head(house27)

categories.1820
colnames(house17)
