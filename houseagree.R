###
###
###First House
library(pscl)
house.1<-readKH("ftp://voteview.com/dtaord/hou01kh.ord")
lopn<-.025*house.1$n
lopn
house.1<-dropUnanimous(house.1,lop=lopn)
summary(house.1)

h.1<-house.1$votes ##First Congress Roll Call Votes
h.1<-h.1[2:nrow(h.1),] ##Dropping President
h.1[h.1==0]<-NA ##Converting 0s to NAs
h.1<-na.omit(h.1) ##Omitting 0s aka Not in Legislature for one or more votes

legnames<-as.matrix(rownames(h.1))
legnames

agree.mat <- function(X){
  
  X <- t(X) # put subjects on columns
  n <- ncol(X)
  A <- matrix(NA, n, n)
  for (i in 1:n){
    A[i,] <- apply(X[,i] == X & X!=9, 2, sum)
  }
  A <- A
  return(A)
}

##This is a shortened example to see how the code is working.
##Appears to be working properly. 
##I show the pure votes, then the calculated agreement scores, 
##which seem to be correct if we restrict attention to first five votes
agt<-agree.mat(head(h.1[,1:5]))
rownames(agt)<-rownames(head(h.1[,1:5]))
colnames(agt)<-rownames(head(h.1[,1:5]))
agt.1<-agt/ncol(head(h.1[,1:5]))
agt.1
head(h.1[,1:5])
##^Just looking at first five votes and first six 
##legislators to make sure counts are right

Ano <- agree.mat(h.1) ##For all 107 votes in first House
rownames(Ano)<-rownames(h.1)
colnames(Ano)<-rownames(h.1)

A <- Ano/ncol(h.1) ##percentage of times they agree

data <- as.data.frame(A) ##First Congress agreement scores as data frame

n <- length(data)

head(data)##look at agreement scores with names listed
dimnames(data)
# Make an empty vector
pairdata_full <- matrix(NA,(n*(n-1)/2),1)
pairdata <- (data[(1+1):n,1])
name_one <- matrix(1,(n-1),1)
name_two <- matrix(2:n, (n-1),1)

# Fill in the data

for (i in 2:n) {
  if ((i+1)<=n){
    pairdata <- c(pairdata, data[(1+i):n,i])
    names <- length(data[(1+i):n,i])
    name_one <- c(name_one, matrix(i,names,1))
    name_two <- c(name_two, matrix((i+1):n, names,1))
  }
}

pairdata_full[,1] <- t(pairdata)
a.h1 <- as.data.frame(pairdata_full)
colnames(a.h1) <- c("rc_agree")
library(foreign)
write.dta(a.h1,"agreefirsthouse.dta")
head(a.h1)
head(data)
summary(data)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G <- graph.adjacency(A,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G)
## Match up the scores to the vector of names
centrality.h1 <- alpha.centrality(G)
## Export the dataset
write.dta(as.data.frame(centrality.h1),"centrality_h1.dta")
plot(density.default(x=alpha.centrality(G)))

###
###
###Second House
house.2<-readKH("ftp://voteview.com/dtaord/hou02kh.ord")
lopn2<-.025*house.2$n
lopn2
house.2<-dropUnanimous(house.2,lop=lopn2)
summary(house.2)

h.2<-house.2$votes ##Second Congress Roll Call Votes
h.2<-h.2[2:nrow(h.2),] ##Dropping President
h.2[h.2==0]<-NA ##Converting 0s to NAs
h.2<-na.omit(h.2) ##Omitting 0s aka Not in Legislature for one or more votes

legnames2<-rownames(h.2)

Ano2 <- agree.mat(h.2) ##For all 101 votes in first House
rownames(Ano2)<-rownames(h.2)
colnames(Ano2)<-rownames(h.2)

A2 <- Ano2/ncol(h.2) ##percentage of times they agree
data2 <- as.data.frame(A2) ##First Congress agreement scores as data frame

n2 <- length(data2)

head(data2)##look at agreement scores with names listed
dimnames(data2)
# Make an empty vector
pairdata_full2 <- matrix(NA,(n2*(n2-1)/2),1)
pairdata2 <- (data2[(1+1):n2,1])
name_one2 <- matrix(1,(n2-1),1)
name_two2 <- matrix(2:n2, (n2-1),1)

# Fill in the data

for (i in 2:n2) {
  if ((i+1)<=n2){
    pairdata2 <- c(pairdata2, data2[(1+i):n2,i])
    names2 <- length(data2[(1+i):n2,i])
    name_one2 <- c(name_one2, matrix(i,names2,1))
    name_two2 <- c(name_two2, matrix((i+1):n2, names2,1))
  }
}

pairdata_full2[,1] <- t(pairdata2)

a.h2 <- as.data.frame(pairdata_full2)
colnames(a.h2) <- c("rc_agree")
write.dta(a.h2,"agreesecondhouse.dta")
head(a.h2)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G2 <- graph.adjacency(A2,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G2)
## Match up the scores to the vector of names
centrality.h2 <- alpha.centrality(G2)
## Export the dataset
write.dta(as.data.frame(centrality.h2),"centrality_h2.dta")
plot(density.default(x=alpha.centrality(G2)))

####
####
####
###Third House

house.3<-readKH("ftp://voteview.com/dtaord/hou03kh.ord")
lopn3<-.025*house.3$n
lopn3
house.3<-dropUnanimous(house.3,lop=lopn3)
summary(house.3)

h.3<-house.3$votes ##Second Congress Roll Call Votes
h.3<-h.3[2:nrow(h.3),] ##Dropping President
h.3[h.3==0]<-NA ##Converting 0s to NAs
h.3<-na.omit(h.3) ##Omitting 0s aka Not in Legislature for one or more votes

legnames3<-rownames(h.3)##Names of legislators

Ano3 <- agree.mat(h.3) ##For all 101 votes in first House
rownames(Ano3)<-rownames(h.3)
colnames(Ano3)<-rownames(h.3)

A3 <- Ano3/ncol(h.3) ##percentage of times they agree
data3 <- as.data.frame(A3) ##First Congress agreement scores as data frame

n3 <- length(data3)

head(data3)##look at agreement scores with names listed
dimnames(data3)
# Make an empty vector
pairdata_full3 <- matrix(NA,(n3*(n3-1)/2),1)
pairdata3 <- (data3[(1+1):n3,1])
name_one3 <- matrix(1,(n3-1),1)
name_two3 <- matrix(2:n3, (n3-1),1)

# Fill in the data

for (i in 2:n3) {
  if ((i+1)<=n3){
    pairdata3 <- c(pairdata3, data3[(1+i):n3,i])
    names3 <- length(data3[(1+i):n3,i])
    name_one3 <- c(name_one3, matrix(i,names3,1))
    name_two3 <- c(name_two3, matrix((i+1):n3, names3,1))
  }
}

pairdata_full3[,1] <- t(pairdata3)

a.h3 <- as.data.frame(pairdata_full3)
colnames(a.h3) <- c("rc_agree")
write.dta(a.h3,"agreethirdhouse.dta")
head(a.h3)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G3 <- graph.adjacency(A3,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G3)
## Match up the scores to the vector of names
centrality.h3 <- alpha.centrality(G3)
## Export the dataset
write.dta(as.data.frame(centrality.h3),"centrality_h3.dta")
plot(density.default(x=alpha.centrality(G3)))

###
###
###
###Fourth House

house.4<-readKH("ftp://voteview.com/dtaord/hou04kh.ord")
lopn4<-.025*house.4$n
lopn4
house.4<-dropUnanimous(house.4,lop=lopn4)
summary(house.4)

h.4<-house.4$votes ##Fourth Congress Roll Call Votes
h.4<-h.4[2:nrow(h.4),] ##Dropping President
h.4[h.4==0]<-NA ##Converting 0s to NAs
h.4<-na.omit(h.4) ##Omitting 0s aka Not in Legislature for one or more votes

legnames4<-rownames(h.4)##Names of legislators

Ano4 <- agree.mat(h.4) ##For all votes in fourth House
rownames(Ano4)<-rownames(h.4)
colnames(Ano4)<-rownames(h.4)

A4 <- Ano4/ncol(h.4) ##percentage of times they agree
data4 <- as.data.frame(A4) ##First Congress agreement scores as data frame

n4 <- length(data4)

head(data4)##look at agreement scores with names listed
dimnames(data4)
# Make an empty vector
pairdata_full4 <- matrix(NA,(n4*(n4-1)/2),1)
pairdata4 <- (data4[(1+1):n4,1])
name_one4 <- matrix(1,(n4-1),1)
name_two4 <- matrix(2:n4, (n4-1),1)

# Fill in the data

for (i in 2:n4) {
  if ((i+1)<=n4){
    pairdata4 <- c(pairdata4, data4[(1+i):n4,i])
    names4 <- length(data4[(1+i):n4,i])
    name_one4 <- c(name_one4, matrix(i,names4,1))
    name_two4 <- c(name_two4, matrix((i+1):n4, names4,1))
  }
}

pairdata_full4[,1] <- t(pairdata4)

a.h4 <- as.data.frame(pairdata_full4)
colnames(a.h4) <- c("rc_agree")
write.dta(a.h4,"agreefourthhouse.dta")
head(a.h4)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G4 <- graph.adjacency(A4,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G4)
## Match up the scores to the vector of names
centrality.h4 <-alpha.centrality(G4)
## Export the dataset
write.dta(as.data.frame(centrality.h4),"centrality_h4.dta")
plot(density.default(x=alpha.centrality(G4)))

###
###
###
###Fifth House

house.5<-readKH("ftp://voteview.com/dtaord/hou05kh.ord")
lopn5<-.025*house.5$n
lopn5
house.5<-dropUnanimous(house.5,lop=lopn5)
summary(house.5)

h.5<-house.5$votes ##Fifth Congress Roll Call Votes
h.5<-h.5[2:nrow(h.5),] ##Dropping President
h.5[h.5==0]<-NA ##Converting 0s to NAs
h.5<-na.omit(h.5) ##Omitting 0s aka Not in Legislature for one or more votes

legnames5<-rownames(h.5)##Names of legislators

Ano5 <- agree.mat(h.5) ##For votes in fifth House
rownames(Ano5)<-rownames(h.5)
colnames(Ano5)<-rownames(h.5)

A5 <- Ano5/ncol(h.5) ##percentage of times they agree
data5 <- as.data.frame(A5) ##First Congress agreement scores as data frame

n5 <- length(data5)

head(data5)##look at agreement scores with names listed
dimnames(data5)
# Make an empty vector
pairdata_full5 <- matrix(NA,(n5*(n5-1)/2),1)
pairdata5 <- (data5[(1+1):n5,1])
name_one5 <- matrix(1,(n5-1),1)
name_two5 <- matrix(2:n5, (n5-1),1)

# Fill in the data

for (i in 2:n5) {
  if ((i+1)<=n5){
    pairdata5 <- c(pairdata5, data5[(1+i):n5,i])
    names5 <- length(data5[(1+i):n5,i])
    name_one5 <- c(name_one5, matrix(i,names5,1))
    name_two5 <- c(name_two5, matrix((i+1):n5, names5,1))
  }
}

pairdata_full5[,1] <- t(pairdata5)

a.h5 <- as.data.frame(pairdata_full5)
colnames(a.h5) <- c("rc_agree")
write.dta(a.h5,"agreefifthhouse.dta")
head(a.h5)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G5 <- graph.adjacency(A5,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G5)
## Match up the scores to the vector of names
centrality.h5 <- alpha.centrality(G5)
## Export the dataset
write.dta(as.data.frame(centrality.h5),"centrality_h5.dta")
plot(density.default(x=alpha.centrality(G5)))

###
###
###
###Sixth House

house.6<-readKH("ftp://voteview.com/dtaord/hou06kh.ord")
lopn6<-.025*house.6$n
lopn6
house.6<-dropUnanimous(house.6,lop=lopn6)
summary(house.6)

h.6<-house.6$votes ##sixth Congress Roll Call Votes
h.6<-h.6[2:nrow(h.6),] ##Dropping President
h.6[h.6==0]<-NA ##Converting 0s to NAs
h.6<-na.omit(h.6) ##Omitting 0s aka Not in Legislature for one or more votes

legnames6<-rownames(h.6)##Names of legislators

Ano6 <- agree.mat(h.6) ##For all votes in sixth House
rownames(Ano6)<-rownames(h.6)
colnames(Ano6)<-rownames(h.6)

A6 <- Ano6/ncol(h.6) ##percentage of times they agree
data6 <- as.data.frame(A6) ##First Congress agreement scores as data frame

n6<- length(data6)

head(data6)##look at agreement scores with names listed
dimnames(data6)
# Make an empty vector
pairdata_full6 <- matrix(NA,(n6*(n6-1)/2),1)
pairdata6<- (data6[(1+1):n6,1])
name_one6 <- matrix(1,(n6-1),1)
name_two6 <- matrix(2:n6, (n6-1),1)

# Fill in the data

for (i in 2:n6) {
  if ((i+1)<=n6){
    pairdata6<- c(pairdata6, data6[(1+i):n6,i])
    names6<- length(data6[(1+i):n6,i])
    name_one6 <- c(name_one6, matrix(i,names6,1))
    name_two6 <- c(name_two6, matrix((i+1):n6, names6,1))
  }
}

pairdata_full6[,1] <- t(pairdata6)

a.h6<- as.data.frame(pairdata_full6)
colnames(a.h6) <- c("rc_agree")
write.dta(a.h6,"agreesixthhouse.dta")
head(a.h6)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G6 <- graph.adjacency(A6,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G6)
## Match up the scores to the vector of names
centrality.h6 <- alpha.centrality(G6)
## Export the dataset
write.dta(as.data.frame(centrality.h6),"centrality_h6.dta")
plot(density.default(x=alpha.centrality(G6)))

###
###
###
###Seventh House

house.7<-readKH("ftp://voteview.com/dtaord/hou07kh.ord")
lopn7<-.025*house.7$n
lopn7
house.7<-dropUnanimous(house.7,lop=lopn7)
summary(house.7)

h.7<-house.7$votes ##Second Congress Roll Call Votes
h.7<-h.7[2:nrow(h.7),] ##Dropping President
h.7[h.7==0]<-NA ##Converting 0s to NAs
h.7<-na.omit(h.7) ##Omitting 0s aka Not in Legislature for one or more votes

legnames7<-rownames(h.7)##Names of legislators

Ano7 <- agree.mat(h.7) ##For all 101 votes in first House
rownames(Ano7)<-rownames(h.7)
colnames(Ano7)<-rownames(h.7)

A7 <- Ano7/ncol(h.7) ##percentage of times they agree
data7 <- as.data.frame(A7) ##First Congress agreement scores as data frame

n7 <- length(data7)

head(data7)##look at agreement scores with names listed
dimnames(data7)
# Make an empty vector
pairdata_full7 <- matrix(NA,(n7*(n7-1)/2),1)
pairdata7 <- (data7[(1+1):n7,1])
name_one7 <- matrix(1,(n7-1),1)
name_two7 <- matrix(2:n7, (n7-1),1)

# Fill in the data

for (i in 2:n7) {
  if ((i+1)<=n7){
    pairdata7 <- c(pairdata7, data7[(1+i):n7,i])
    names7 <- length(data7[(1+i):n7,i])
    name_one7 <- c(name_one7, matrix(i,names7,1))
    name_two7 <- c(name_two7, matrix((i+1):n7, names7,1))
  }
}

pairdata_full7[,1] <- t(pairdata7)

a.h7 <- as.data.frame(pairdata_full7)
colnames(a.h7) <- c("rc_agree")
write.dta(a.h7,"agreeseventhhouse.dta")
head(a.h7)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G7 <- graph.adjacency(A7,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G7)
## Match up the scores to the vector of names
centrality.h7 <- alpha.centrality(G7)
## Export the dataset
write.dta(as.data.frame(centrality.h7),"centrality_h7.dta")
plot(density.default(x=alpha.centrality(G7)))

###
###
###
###eighth House

house.8<-readKH("ftp://voteview.com/dtaord/hou08kh.ord")
lopn8<-.025*house.8$n
lopn8
house.8<-dropUnanimous(house.8,lop=lopn8)
summary(house.8)

h.8<-house.8$votes ##tenth Congress Roll Call Votes
h.8<-h.8[2:nrow(h.8),] ##Dropping President
h.8[h.8==0]<-NA ##Converting 0s to NAs
h.8<-na.omit(h.8) ##Omitting 0s aka Not in Legislature for one or more votes

legnames8<-rownames(h.8)##Names of legislators

Ano8 <- agree.mat(h.8) ##For all 101 votes in first House
rownames(Ano8)<-rownames(h.8)
colnames(Ano8)<-rownames(h.8)

A8 <- Ano8/ncol(h.8) ##percentage of times they agree
data8 <- as.data.frame(A8) ##First Congress agreement scores as data frame

n8 <- length(data8)

head(data8)##look at agreement scores with names listed
dimnames(data8)
# Make an empty vector
pairdata_full8 <- matrix(NA,(n8*(n8-1)/2),1)
pairdata8 <- (data8[(1+1):n8,1])
name_one8 <- matrix(1,(n8-1),1)
name_two8 <- matrix(2:n8, (n8-1),1)

# Fill in the data

for (i in 2:n8) {
  if ((i+1)<=n8){
    pairdata8 <- c(pairdata8, data8[(1+i):n8,i])
    names8 <- length(data8[(1+i):n8,i])
    name_one8 <- c(name_one8, matrix(i,names8,1))
    name_two8 <- c(name_two8, matrix((i+1):n8, names8,1))
  }
}

pairdata_full8[,1] <- t(pairdata8)

a.h8 <- as.data.frame(pairdata_full8)
colnames(a.h8) <- c("rc_agree")
write.dta(a.h8,"agreeeighthhouse.dta")
head(a.h8)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G8 <- graph.adjacency(A8,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G8)
## Match up the scores to the vector of names
centrality.h8 <- alpha.centrality(G8)
## Export the dataset
write.dta(as.data.frame(centrality.h8),"centrality_h8.dta")
plot(density.default(x=alpha.centrality(G8)))

###
###
###
###ninth House

house.9<-readKH("ftp://voteview.com/dtaord/hou09kh.ord")
lopn9<-.025*house.9$n
lopn9
house.9<-dropUnanimous(house.9,lop=lopn9)
summary(house.9)

h.9<-house.9$votes ##Ninth Congress Roll Call Votes
h.9<-h.9[2:nrow(h.9),] ##Dropping President
h.9[h.9==0]<-NA ##Converting 0s to NAs
h.9<-na.omit(h.9) ##Omitting 0s aka Not in Legislature for one or more votes

legnames9<-rownames(h.9)##Names of legislators

Ano9 <- agree.mat(h.9) ##For all 101 votes in first House
rownames(Ano9)<-rownames(h.9)
colnames(Ano9)<-rownames(h.9)

A9 <- Ano9/ncol(h.9) ##percentage of times they agree
data9 <- as.data.frame(A9) ##First Congress agreement scores as data frame

n9 <- length(data9)

head(data9)##look at agreement scores with names listed
dimnames(data9)
# Make an empty vector
pairdata_full9 <- matrix(NA,(n9*(n9-1)/2),1)
pairdata9 <- (data9[(1+1):n9,1])
name_one9 <- matrix(1,(n9-1),1)
name_two9 <- matrix(2:n9, (n9-1),1)

# Fill in the data

for (i in 2:n9) {
  if ((i+1)<=n9){
    pairdata9 <- c(pairdata9, data9[(1+i):n9,i])
    names9 <- length(data9[(1+i):n9,i])
    name_one9 <- c(name_one9, matrix(i,names9,1))
    name_two9 <- c(name_two9, matrix((i+1):n9, names9,1))
  }
}

pairdata_full9[,1] <- t(pairdata9)

a.h9 <- as.data.frame(pairdata_full9)
colnames(a.h9) <- c("rc_agree")
write.dta(a.h9,"agreeninthhouse.dta")
head(a.h9)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G9 <- graph.adjacency(A9,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G9)
## Match up the scores to the vector of names
centrality.h9 <- alpha.centrality(G9)
## Export the dataset
write.dta(as.data.frame(centrality.h9),"centrality_h9.dta")
plot(density.default(x=alpha.centrality(G9)))

###
###
###
###tenth House

house.10<-readKH("ftp://voteview.com/dtaord/hou10kh.ord")
lopn10<-.025*house.10$n
lopn10
house.10<-dropUnanimous(house.10,lop=lopn10)
summary(house.10)

h.10<-house.10$votes ##Second Congress Roll Call Votes
h.10<-h.10[2:nrow(h.10),] ##Dropping President
h.10[h.10==0]<-NA ##Converting 0s to NAs
h.10<-na.omit(h.10) ##Omitting 0s aka Not in Legislature for one or more votes

legnames10<-rownames(h.10)##Names of legislators

Ano10 <- agree.mat(h.10) ##For all 101 votes in first House
rownames(Ano10)<-rownames(h.10)
colnames(Ano10)<-rownames(h.10)

A10 <- Ano10/ncol(h.10) ##percentage of times they agree
data10 <- as.data.frame(A10) ##First Congress agreement scores as data frame

n10 <- length(data10)

head(data10)##look at agreement scores with names listed
dimnames(data10)
# Make an empty vector
pairdata_full10 <- matrix(NA,(n10*(n10-1)/2),1)
pairdata10 <- (data10[(1+1):n10,1])
name_one10 <- matrix(1,(n10-1),1)
name_two10 <- matrix(2:n10, (n10-1),1)

# Fill in the data

for (i in 2:n10) {
  if ((i+1)<=n10){
    pairdata10 <- c(pairdata10, data10[(1+i):n10,i])
    names10 <- length(data10[(1+i):n10,i])
    name_one10 <- c(name_one10, matrix(i,names10,1))
    name_two10 <- c(name_two10, matrix((i+1):n10, names10,1))
  }
}

pairdata_full10[,1] <- t(pairdata10)

a.h10 <- as.data.frame(pairdata_full10)
colnames(a.h10) <- c("rc_agree")
write.dta(a.h10,"agreetenthhouse.dta")
head(a.h10)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G10 <- graph.adjacency(A10,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G10)
## Match up the scores to the vector of names
centrality.h10 <- alpha.centrality(G10)
## Export the dataset
write.dta(as.data.frame(centrality.h10),"centrality_h10.dta")
plot(density.default(x=alpha.centrality(G10)))

###
###
###
###eleventh House

house.11<-readKH("ftp://voteview.com/dtaord/hou11kh.ord")
lopn11<-.025*house.11$n
lopn11
house.11<-dropUnanimous(house.11,lop=lopn11)
summary(house.11)

h.11<-house.11$votes ##Second Congress Roll Call Votes
h.11<-h.11[2:nrow(h.11),] ##Dropping President
h.11[h.11==0]<-NA ##Converting 0s to NAs
h.11<-na.omit(h.11) ##Omitting 0s aka Not in Legislature for one or more votes

legnames11<-rownames(h.11)##Names of legislators

Ano11 <- agree.mat(h.11) ##For all 101 votes in first House
rownames(Ano11)<-rownames(h.11)
colnames(Ano11)<-rownames(h.11)

A11 <- Ano11/ncol(h.11) ##percentage of times they agree
data11 <- as.data.frame(A11) ##First Congress agreement scores as data frame

n11 <- length(data11)

head(data11)##look at agreement scores with names listed
dimnames(data11)
# Make an empty vector
pairdata_full11 <- matrix(NA,(n11*(n11-1)/2),1)
pairdata11 <- (data11[(1+1):n11,1])
name_one11 <- matrix(1,(n11-1),1)
name_two11 <- matrix(2:n11, (n11-1),1)

# Fill in the data

for (i in 2:n11) {
  if ((i+1)<=n11){
    pairdata11 <- c(pairdata11, data11[(1+i):n11,i])
    names11 <- length(data11[(1+i):n11,i])
    name_one11 <- c(name_one11, matrix(i,names11,1))
    name_two11 <- c(name_two11, matrix((i+1):n11, names11,1))
  }
}

pairdata_full11[,1] <- t(pairdata11)

a.h11 <- as.data.frame(pairdata_full11)
colnames(a.h11) <- c("rc_agree")
write.dta(a.h11,"agreeeleventhhouse.dta")
head(a.h11)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G11 <- graph.adjacency(A11,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G11)
## Match up the scores to the vector of names
centrality.h11 <- alpha.centrality(G11)
## Export the dataset
write.dta(as.data.frame(centrality.h11),"centrality_h11.dta")
plot(density.default(x=alpha.centrality(G11)))

###
###
###
###twelfth House

house.12<-readKH("ftp://voteview.com/dtaord/hou12kh.ord")
lopn12<-.025*house.12$n
lopn12
house.12<-dropUnanimous(house.12,lop=lopn12)
summary(house.12)

h.12<-house.12$votes ##Second Congress Roll Call Votes
h.12<-h.12[2:nrow(h.12),] ##Dropping President
h.12[h.12==0]<-NA ##Converting 0s to NAs
h.12<-na.omit(h.12) ##Omitting 0s aka Not in Legislature for one or more votes

legnames12<-rownames(h.12)##Names of legislators

Ano12 <- agree.mat(h.12) ##For all votes in twelfth House
rownames(Ano12)<-rownames(h.12)
colnames(Ano12)<-rownames(h.12)

A12 <- Ano12/ncol(h.12) ##percentage of times they agree
data12 <- as.data.frame(A12) ##First Congress agreement scores as data frame

n12 <- length(data12)

head(data12)##look at agreement scores with names listed
dimnames(data12)
# Make an empty vector
pairdata_full12 <- matrix(NA,(n12*(n12-1)/2),1)
pairdata12 <- (data12[(1+1):n12,1])
name_one12 <- matrix(1,(n12-1),1)
name_two12 <- matrix(2:n12, (n12-1),1)

# Fill in the data

for (i in 2:n12) {
  if ((i+1)<=n12){
    pairdata12 <- c(pairdata12, data12[(1+i):n12,i])
    names12 <- length(data12[(1+i):n12,i])
    name_one12 <- c(name_one12, matrix(i,names12,1))
    name_two12 <- c(name_two12, matrix((i+1):n12, names12,1))
  }
}

pairdata_full12[,1] <- t(pairdata12)

a.h12 <- as.data.frame(pairdata_full12)
colnames(a.h12) <- c("rc_agree")
write.dta(a.h12,"agreetwelfthhouse.dta")
head(a.h12)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G12 <- graph.adjacency(A12,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G12)
## Match up the scores to the vector of names
centrality.h12 <-alpha.centrality(G12)
## Export the dataset
write.dta(as.data.frame(centrality.h12),"centrality_h12.dta")
plot(density.default(x=alpha.centrality(G12)))

###
###
###thirteenth House

house.13<-readKH("ftp://voteview.com/dtaord/hou13kh.ord")
lopn13<-.025*house.13$n
lopn13
house.13<-dropUnanimous(house.13,lop=lopn13)
summary(house.13)

h.13<-house.13$votes ##Second Congress Roll Call Votes
h.13<-h.13[2:nrow(h.13),] ##Dropping President
h.13[h.13==0]<-NA ##Converting 0s to NAs
h.13<-na.omit(h.13) ##Omitting 0s aka Not in Legislature for one or more votes

legnames13<-rownames(h.13)##Names of legislators

Ano13 <- agree.mat(h.13) ##For all votes in thirteenth House
rownames(Ano13)<-rownames(h.13)
colnames(Ano13)<-rownames(h.13)

A13 <- Ano13/ncol(h.13) ##percentage of times they agree
data13 <- as.data.frame(A13) ##First Congress agreement scores as data frame

n13 <- length(data13)

head(data13)##look at agreement scores with names listed
dimnames(data13)
# Make an empty vector
pairdata_full13 <- matrix(NA,(n13*(n13-1)/2),1)
pairdata13 <- (data13[(1+1):n13,1])
name_one13 <- matrix(1,(n13-1),1)
name_two13 <- matrix(2:n13, (n13-1),1)

# Fill in the data

for (i in 2:n13) {
  if ((i+1)<=n13){
    pairdata13 <- c(pairdata13, data13[(1+i):n13,i])
    names13 <- length(data13[(1+i):n13,i])
    name_one13 <- c(name_one13, matrix(i,names13,1))
    name_two13 <- c(name_two13, matrix((i+1):n13, names13,1))
  }
}

pairdata_full13[,1] <- t(pairdata13)

a.h13 <- as.data.frame(pairdata_full13)
colnames(a.h13) <- c("rc_agree")
write.dta(a.h13,"agreethirteenthhouse.dta")
head(a.h13)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G13 <- graph.adjacency(A13,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G13)
## Match up the scores to the vector of names
centrality.h13 <- alpha.centrality(G13)
## Export the dataset
write.dta(as.data.frame(centrality.h13),"centrality_h13.dta")
plot(density.default(x=alpha.centrality(G13)))

###
###
###fourteenth House

house.14<-readKH("ftp://voteview.com/dtaord/hou14kh.ord")
lopn14<-.025*house.14$n
lopn14
house.14<-dropUnanimous(house.14,lop=lopn14)
summary(house.14)

h.14<-house.14$votes ##Second Congress Roll Call Votes
h.14<-h.14[2:nrow(h.14),] ##Dropping President
h.14[h.14==0]<-NA ##Converting 0s to NAs
h.14<-na.omit(h.14) ##Omitting 0s aka Not in Legislature for one or more votes

legnames14<-rownames(h.14)##Names of legislators

Ano14 <- agree.mat(h.14) ##For all votes in fourteenth House
rownames(Ano14)<-rownames(h.14)
colnames(Ano14)<-rownames(h.14)

A14 <- Ano14/ncol(h.14) ##percentage of times they agree
data14 <- as.data.frame(A14) ##First Congress agreement scores as data frame

n14 <- length(data14)

head(data14)##look at agreement scores with names listed
dimnames(data14)
# Make an empty vector
pairdata_full14 <- matrix(NA,(n14*(n14-1)/2),1)
pairdata14 <- (data14[(1+1):n14,1])
name_one14 <- matrix(1,(n14-1),1)
name_two14 <- matrix(2:n14, (n14-1),1)

# Fill in the data

for (i in 2:n14) {
  if ((i+1)<=n14){
    pairdata14 <- c(pairdata14, data14[(1+i):n14,i])
    names14 <- length(data14[(1+i):n14,i])
    name_one14 <- c(name_one14, matrix(i,names14,1))
    name_two14 <- c(name_two14, matrix((i+1):n14, names14,1))
  }
}

pairdata_full14[,1] <- t(pairdata14)

a.h14 <- as.data.frame(pairdata_full14)
colnames(a.h14) <- c("rc_agree")
write.dta(a.h14,"agreefourteenthhouse.dta")
head(a.h14)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G14 <- graph.adjacency(A14,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G14)
## Match up the scores to the vector of names
centrality.h14 <- (alpha.centrality(G14))
## Export the dataset
write.dta(as.data.frame(centrality.h14),"centrality_h14.dta")
plot(density.default(x=alpha.centrality(G14)))

###
###
###fifteenth House

house.15<-readKH("ftp://voteview.com/dtaord/hou15kh.ord")
lopn15<-.025*house.15$n
lopn15
house.15<-dropUnanimous(house.15,lop=lopn15)
summary(house.15)

h.15<-house.15$votes ##Second Congress Roll Call Votes
h.15<-h.15[2:nrow(h.15),] ##Dropping President
h.15[h.15==0]<-NA ##Converting 0s to NAs
h.15<-na.omit(h.15) ##Omitting 0s aka Not in Legislature for one or more votes

legnames15<-rownames(h.15)##Names of legislators

Ano15 <- agree.mat(h.15) ##For all votes in fifteenth House
rownames(Ano15)<-rownames(h.15)
colnames(Ano15)<-rownames(h.15)

A15 <- Ano15/ncol(h.15) ##percentage of times they agree
data15 <- as.data.frame(A15) ##First Congress agreement scores as data frame

n15 <- length(data15)

head(data15)##look at agreement scores with names listed
dimnames(data15)
# Make an empty vector
pairdata_full15 <- matrix(NA,(n15*(n15-1)/2),1)
pairdata15 <- (data15[(1+1):n15,1])
name_one15 <- matrix(1,(n15-1),1)
name_two15 <- matrix(2:n15, (n15-1),1)

# Fill in the data

for (i in 2:n15) {
  if ((i+1)<=n15){
    pairdata15 <- c(pairdata15, data15[(1+i):n15,i])
    names15 <- length(data15[(1+i):n15,i])
    name_one15 <- c(name_one15, matrix(i,names15,1))
    name_two15 <- c(name_two15, matrix((i+1):n15, names15,1))
  }
}

pairdata_full15[,1] <- t(pairdata15)

a.h15 <- as.data.frame(pairdata_full15)
colnames(a.h15) <- c("rc_agree")
write.dta(a.h15,"agreefifteenthhouse.dta")
head(a.h15)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G15 <- graph.adjacency(A15,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G15)
## Match up the scores to the vector of names
centrality.h15 <- alpha.centrality(G15)
## Export the dataset
write.dta(as.data.frame(centrality.h15),"centrality_h15.dta")
plot(density.default(x=alpha.centrality(G15)))

###
###
###sixteenth House

house.16<-readKH("ftp://voteview.com/dtaord/hou16kh.ord")
lopn16<-.025*house.16$n
lopn16
house.16<-dropUnanimous(house.16,lop=lopn16)
summary(house.16)

h.16<-house.16$votes ##Sixteenth Congress Roll Call Votes
h.16<-h.16[2:nrow(h.16),] ##Dropping President
h.16[h.16==0]<-NA ##Converting 0s to NAs
h.16<-na.omit(h.16) ##Omitting 0s aka Not in Legislature for one or more votes

legnames16<-rownames(h.16)##Names of legislators

Ano16 <- agree.mat(h.16) ##For all votes in sixteenth House
rownames(Ano16)<-rownames(h.16)
colnames(Ano16)<-rownames(h.16)

A16 <- Ano16/ncol(h.16) ##percentage of times they agree
data16 <- as.data.frame(A16) ##First Congress agreement scores as data frame

n16 <- length(data16)

head(data16)##look at agreement scores with names listed
dimnames(data16)
# Make an empty vector
pairdata_full16 <- matrix(NA,(n16*(n16-1)/2),1)
pairdata16 <- (data16[(1+1):n16,1])
name_one16 <- matrix(1,(n16-1),1)
name_two16 <- matrix(2:n16, (n16-1),1)

# Fill in the data

for (i in 2:n16) {
  if ((i+1)<=n16){
    pairdata16 <- c(pairdata16, data16[(1+i):n16,i])
    names16 <- length(data16[(1+i):n16,i])
    name_one16 <- c(name_one16, matrix(i,names16,1))
    name_two16 <- c(name_two16, matrix((i+1):n16, names16,1))
  }
}

pairdata_full16[,1] <- t(pairdata16)

a.h16 <- as.data.frame(pairdata_full16)
colnames(a.h16) <- c("rc_agree")
write.dta(a.h16,"agreesixteenthhouse.dta")
head(a.h16)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G16 <- graph.adjacency(A16,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G16)
## Match up the scores to the vector of names
centrality.h16 <- alpha.centrality(G16)
## Export the dataset
write.dta(as.data.frame(centrality.h16),"centrality_h16.dta")
plot(density.default(x=alpha.centrality(G16)))

###
###
###seventeenth House

house.17<-readKH("ftp://voteview.com/dtaord/hou17kh.ord")
lopn17<-.025*house.17$n
lopn17
house.17<-dropUnanimous(house.17,lop=lopn17)
summary(house.17)

h.17<-house.17$votes ##Second Congress Roll Call Votes
h.17<-h.17[2:nrow(h.17),] ##Dropping President
h.17[h.17==0]<-NA ##Converting 0s to NAs
h.17<-na.omit(h.17) ##Omitting 0s aka Not in Legislature for one or more votes

legnames17<-rownames(h.17)##Names of legislators

Ano17 <- agree.mat(h.17) ##For all votes in seventeenth House
rownames(Ano17)<-rownames(h.17)
colnames(Ano17)<-rownames(h.17)

A17 <- Ano17/ncol(h.17) ##percentage of times they agree
data17 <- as.data.frame(A17) ##First Congress agreement scores as data frame

n17 <- length(data17)

head(data17)##look at agreement scores with names listed
dimnames(data17)
# Make an empty vector
pairdata_full17 <- matrix(NA,(n17*(n17-1)/2),1)
pairdata17 <- (data17[(1+1):n17,1])
name_one17 <- matrix(1,(n17-1),1)
name_two17 <- matrix(2:n17, (n17-1),1)

# Fill in the data

for (i in 2:n17) {
  if ((i+1)<=n17){
    pairdata17 <- c(pairdata17, data17[(1+i):n17,i])
    names17 <- length(data17[(1+i):n17,i])
    name_one17 <- c(name_one17, matrix(i,names17,1))
    name_two17 <- c(name_two17, matrix((i+1):n17, names17,1))
  }
}

pairdata_full17[,1] <- t(pairdata17)

a.h17 <- as.data.frame(pairdata_full17)
colnames(a.h17) <- c("rc_agree")
write.dta(a.h17,"agreeseventeenthhouse.dta")
head(a.h17)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G17 <- graph.adjacency(A17,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G17)
## Match up the scores to the vector of names
centrality.h17 <- alpha.centrality(G17)
## Export the dataset
write.dta(as.data.frame(centrality.h17),"centrality_h17.dta")
plot(density.default(x=alpha.centrality(G17)))

###
###
###eighteenth House

house.18<-readKH("ftp://voteview.com/dtaord/hou18kh.ord")
lopn18<-.025*house.18$n
lopn18
house.18<-dropUnanimous(house.18,lop=lopn18)
summary(house.18)

h.18<-house.18$votes ##Second Congress Roll Call Votes
h.18<-h.18[2:nrow(h.18),] ##Dropping President
h.18[h.18==0]<-NA ##Converting 0s to NAs
h.18<-na.omit(h.18) ##Omitting 0s aka Not in Legislature for one or more votes

legnames18<-rownames(h.18)##Names of legislators

Ano18 <- agree.mat(h.18) ##For all votes in eighteenth House
rownames(Ano18)<-rownames(h.18)
colnames(Ano18)<-rownames(h.18)

A18 <- Ano18/ncol(h.18) ##percentage of times they agree
data18 <- as.data.frame(A18) ##First Congress agreement scores as data frame

n18 <- length(data18)

head(data18)##look at agreement scores with names listed
dimnames(data18)
# Make an empty vector
pairdata_full18 <- matrix(NA,(n18*(n18-1)/2),1)
pairdata18 <- (data18[(1+1):n18,1])
name_one18 <- matrix(1,(n18-1),1)
name_two18 <- matrix(2:n18, (n18-1),1)

# Fill in the data

for (i in 2:n18) {
  if ((i+1)<=n18){
    pairdata18 <- c(pairdata18, data18[(1+i):n18,i])
    names18 <- length(data18[(1+i):n18,i])
    name_one18 <- c(name_one18, matrix(i,names18,1))
    name_two18 <- c(name_two18, matrix((i+1):n18, names18,1))
  }
}

pairdata_full18[,1] <- t(pairdata18)

a.h18 <- as.data.frame(pairdata_full18)
colnames(a.h18) <- c("rc_agree")
write.dta(a.h18,"agreeeighteenthhouse.dta")
head(a.h18)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G18 <- graph.adjacency(A18,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G18)
## Match up the scores to the vector of names
centrality.h18 <- alpha.centrality(G18)
## Export the dataset
write.dta(as.data.frame(centrality.h18),"centrality_h18.dta")
plot(density.default(x=alpha.centrality(G18)))

###
###
###nineteenth House

house.19<-readKH("ftp://voteview.com/dtaord/hou19kh.ord")
lopn19<-.025*house.19$n
lopn19
house.19<-dropUnanimous(house.19,lop=lopn19)
summary(house.19)

h.19<-house.19$votes ##Second Congress Roll Call Votes
h.19<-h.19[2:nrow(h.19),] ##Dropping President
h.19[h.19==0]<-NA ##Converting 0s to NAs
h.19<-na.omit(h.19) ##Omitting 0s aka Not in Legislature for one or more votes

legnames19<-rownames(h.19)##Names of legislators

Ano19 <- agree.mat(h.19) ##For all votes in nineteenth House
rownames(Ano19)<-rownames(h.19)
colnames(Ano19)<-rownames(h.19)

A19 <- Ano19/ncol(h.19) ##percentage of times they agree
data19 <- as.data.frame(A19) ##First Congress agreement scores as data frame

n19 <- length(data19)

head(data19)##look at agreement scores with names listed
dimnames(data19)
# Make an empty vector
pairdata_full19 <- matrix(NA,(n19*(n19-1)/2),1)
pairdata19 <- (data19[(1+1):n19,1])
name_one19 <- matrix(1,(n19-1),1)
name_two19 <- matrix(2:n19, (n19-1),1)

# Fill in the data

for (i in 2:n19) {
  if ((i+1)<=n19){
    pairdata19 <- c(pairdata19, data19[(1+i):n19,i])
    names19 <- length(data19[(1+i):n19,i])
    name_one19 <- c(name_one19, matrix(i,names19,1))
    name_two19 <- c(name_two19, matrix((i+1):n19, names19,1))
  }
}

pairdata_full19[,1] <- t(pairdata19)

a.h19 <- as.data.frame(pairdata_full19)
colnames(a.h19) <- c("rc_agree")
write.dta(a.h19,"agreenineteenthhouse.dta")
head(a.h19)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G19 <- graph.adjacency(A19,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G19)
## Match up the scores to the vector of names
centrality.h19 <-alpha.centrality(G19)
## Export the dataset
write.dta(as.data.frame(centrality.h19),"centrality_h19.dta")
plot(density.default(x=alpha.centrality(G19)))

###
###
###twentieth House

house.20<-readKH("ftp://voteview.com/dtaord/hou20kh.ord")
lopn20<-.025*house.20$n
lopn20
house.20<-dropUnanimous(house.20,lop=lopn20)
summary(house.20)

h.20<-house.20$votes ##Second Congress Roll Call Votes
h.20<-h.20[2:nrow(h.20),] ##Dropping President
h.20[h.20==0]<-NA ##Converting 0s to NAs
h.20<-na.omit(h.20) ##Omitting 0s aka Not in Legislature for one or more votes

legnames20<-rownames(h.20)##Names of legislators

Ano20 <- agree.mat(h.20) ##For all votes in twentieth House
rownames(Ano20)<-rownames(h.20)
colnames(Ano20)<-rownames(h.20)

A20 <- Ano20/ncol(h.20) ##percentage of times they agree
data20 <- as.data.frame(A20) ##First Congress agreement scores as data frame

n20 <- length(data20)

head(data20)##look at agreement scores with names listed
dimnames(data20)
# Make an empty vector
pairdata_full20 <- matrix(NA,(n20*(n20-1)/2),1)
pairdata20 <- (data20[(1+1):n20,1])
name_one20 <- matrix(1,(n20-1),1)
name_two20 <- matrix(2:n20, (n20-1),1)

# Fill in the data

for (i in 2:n20) {
  if ((i+1)<=n20){
    pairdata20 <- c(pairdata20, data20[(1+i):n20,i])
    names20 <- length(data20[(1+i):n20,i])
    name_one20 <- c(name_one20, matrix(i,names20,1))
    name_two20 <- c(name_two20, matrix((i+1):n20, names20,1))
  }
}

pairdata_full20[,1] <- t(pairdata20)

a.h20 <- as.data.frame(pairdata_full20)
colnames(a.h20) <- c("rc_agree")
write.dta(a.h20,"agreetwentiethhouse.dta")
head(a.h20)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G20 <- graph.adjacency(A20,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G20)
## Match up the scores to the vector of names
centrality.h20 <- alpha.centrality(G20)
## Export the dataset
write.dta(as.data.frame(centrality.h20),"centrality_h20.dta")
plot(density.default(x=alpha.centrality(G20)))

###
###
###twentyfirst House

house.21<-readKH("ftp://voteview.com/dtaord/hou21kh.ord")
lopn21<-.025*house.21$n
lopn21
house.21<-dropUnanimous(house.21,lop=lopn21)
summary(house.21)

h.21<-house.21$votes ##21 Congress Roll Call Votes
h.21<-h.21[2:nrow(h.21),] ##Dropping President
h.21[h.21==0]<-NA ##Converting 0s to NAs
h.21<-na.omit(h.21) ##Omitting 0s aka Not in Legislature for one or more votes

legnames21<-rownames(h.21)##Names of legislators

Ano21 <- agree.mat(h.21) ##For all votes in twentyfirst House
rownames(Ano21)<-rownames(h.21)
colnames(Ano21)<-rownames(h.21)

A21 <- Ano21/ncol(h.21) ##percentage of times they agree
data21 <- as.data.frame(A21) ##First Congress agreement scores as data frame

n21 <- length(data21)

head(data21)##look at agreement scores with names listed
dimnames(data21)
# Make an empty vector
pairdata_full21 <- matrix(NA,(n21*(n21-1)/2),1)
pairdata21 <- (data21[(1+1):n21,1])
name_one21 <- matrix(1,(n21-1),1)
name_two21 <- matrix(2:n21, (n21-1),1)

# Fill in the data

for (i in 2:n21) {
  if ((i+1)<=n21){
    pairdata21 <- c(pairdata21, data21[(1+i):n21,i])
    names21 <- length(data21[(1+i):n21,i])
    name_one21 <- c(name_one21, matrix(i,names21,1))
    name_two21 <- c(name_two21, matrix((i+1):n21, names21,1))
  }
}

pairdata_full21[,1] <- t(pairdata21)

a.h21 <- as.data.frame(pairdata_full21)
colnames(a.h21) <- c("rc_agree")
write.dta(a.h21,"agreetwentyfirsthouse.dta")
head(a.h21)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G21 <- graph.adjacency(A21,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G21)
## Match up the scores to the vector of names
centrality.h21 <- alpha.centrality(G21)
## Export the dataset
write.dta(as.data.frame(centrality.h21),"centrality_h21.dta")
plot(density.default(x=alpha.centrality(G21)))

###
###
###twentysecond House

house.22<-readKH("ftp://voteview.com/dtaord/hou22kh.ord")
lopn22<-.025*house.22$n
lopn22
house.22<-dropUnanimous(house.22,lop=lopn22)
summary(house.22)

h.22<-house.22$votes ##Second Congress Roll Call Votes
h.22<-h.22[2:nrow(h.22),] ##Dropping President
h.22[h.22==0]<-NA ##Converting 0s to NAs
h.22<-na.omit(h.22) ##Omitting 0s aka Not in Legislature for one or more votes

legnames22<-rownames(h.22)##Names of legislators

Ano22 <- agree.mat(h.22) ##For all votes in twentysecond House
rownames(Ano22)<-rownames(h.22)
colnames(Ano22)<-rownames(h.22)

A22 <- Ano22/ncol(h.22) ##percentage of times they agree
data22 <- as.data.frame(A22) ##First Congress agreement scores as data frame

n22 <- length(data22)

head(data22)##look at agreement scores with names listed
dimnames(data22)
# Make an empty vector
pairdata_full22 <- matrix(NA,(n22*(n22-1)/2),1)
pairdata22 <- (data22[(1+1):n22,1])
name_one22 <- matrix(1,(n22-1),1)
name_two22 <- matrix(2:n22, (n22-1),1)

# Fill in the data

for (i in 2:n22) {
  if ((i+1)<=n22){
    pairdata22 <- c(pairdata22, data22[(1+i):n22,i])
    names22 <- length(data22[(1+i):n22,i])
    name_one22 <- c(name_one22, matrix(i,names22,1))
    name_two22 <- c(name_two22, matrix((i+1):n22, names22,1))
  }
}

pairdata_full22[,1] <- t(pairdata22)

a.h22 <- as.data.frame(pairdata_full22)
colnames(a.h22) <- c("rc_agree")
write.dta(a.h22,"agreetwentysecondhouse.dta")
head(a.h22)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G22 <- graph.adjacency(A22,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G22)
## Match up the scores to the vector of names
centrality.h22 <- alpha.centrality(G22)
## Export the dataset
write.dta(as.data.frame(centrality.h22),"centrality_h22.dta")
plot(density.default(x=alpha.centrality(G22)))

###
###
###twentythird House

house.23<-readKH("ftp://voteview.com/dtaord/hou23kh.ord")
lopn23<-.025*house.23$n
lopn23
house.23<-dropUnanimous(house.23,lop=lopn23)
summary(house.23)

h.23<-house.23$votes ##Second Congress Roll Call Votes
h.23<-h.23[2:nrow(h.23),] ##Dropping President
h.23[h.23==0]<-NA ##Converting 0s to NAs
h.23<-na.omit(h.23) ##Omitting 0s aka Not in Legislature for one or more votes

legnames23<-rownames(h.23)##Names of legislators

Ano23 <- agree.mat(h.23) ##For all votes in twentythird House
rownames(Ano23)<-rownames(h.23)
colnames(Ano23)<-rownames(h.23)

A23 <- Ano23/ncol(h.23) ##percentage of times they agree
data23 <- as.data.frame(A23) ##First Congress agreement scores as data frame

n23 <- length(data23)

head(data23)##look at agreement scores with names listed
dimnames(data23)
# Make an empty vector
pairdata_full23 <- matrix(NA,(n23*(n23-1)/2),1)
pairdata23 <- (data23[(1+1):n23,1])
name_one23 <- matrix(1,(n23-1),1)
name_two23 <- matrix(2:n23, (n23-1),1)

# Fill in the data

for (i in 2:n23) {
  if ((i+1)<=n23){
    pairdata23 <- c(pairdata23, data23[(1+i):n23,i])
    names23 <- length(data23[(1+i):n23,i])
    name_one23 <- c(name_one23, matrix(i,names23,1))
    name_two23 <- c(name_two23, matrix((i+1):n23, names23,1))
  }
}

pairdata_full23[,1] <- t(pairdata23)

a.h23 <- as.data.frame(pairdata_full23)
colnames(a.h23) <- c("rc_agree")
write.dta(a.h23,"agreetwentythirdhouse.dta")
head(a.h23)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G23 <- graph.adjacency(A23,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G23)
## Match up the scores to the vector of names
centrality.h23 <- alpha.centrality(G23)
## Export the dataset
write.dta(as.data.frame(centrality.h23),"centrality_h23.dta")
plot(density.default(x=alpha.centrality(G23)))

###
###
###twentyfourth House

house.24<-readKH("ftp://voteview.com/dtaord/hou24kh.ord")
lopn24<-.025*house.24$n
lopn24
house.24<-dropUnanimous(house.24,lop=lopn24)
summary(house.24)

h.24<-house.24$votes ##Second Congress Roll Call Votes
h.24<-h.24[2:nrow(h.24),] ##Dropping President
h.24[h.24==0]<-NA ##Converting 0s to NAs
h.24<-na.omit(h.24) ##Omitting 0s aka Not in Legislature for one or more votes

legnames24<-rownames(h.24)##Names of legislators

Ano24 <- agree.mat(h.24) ##For all votes in twentyfourth House
rownames(Ano24)<-rownames(h.24)
colnames(Ano24)<-rownames(h.24)

A24 <- Ano24/ncol(h.24) ##percentage of times they agree
data24 <- as.data.frame(A24) ##First Congress agreement scores as data frame

n24 <- length(data24)

head(data24)##look at agreement scores with names listed
dimnames(data24)
# Make an empty vector
pairdata_full24 <- matrix(NA,(n24*(n24-1)/2),1)
pairdata24 <- (data24[(1+1):n24,1])
name_one24 <- matrix(1,(n24-1),1)
name_two24 <- matrix(2:n24, (n24-1),1)

# Fill in the data

for (i in 2:n24) {
  if ((i+1)<=n24){
    pairdata24 <- c(pairdata24, data24[(1+i):n24,i])
    names24 <- length(data24[(1+i):n24,i])
    name_one24 <- c(name_one24, matrix(i,names24,1))
    name_two24 <- c(name_two24, matrix((i+1):n24, names24,1))
  }
}

pairdata_full24[,1] <- t(pairdata24)

a.h24 <- as.data.frame(pairdata_full24)
colnames(a.h24) <- c("rc_agree")
write.dta(a.h24,"agreetwentyfourthhouse.dta")
head(a.h24)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G24 <- graph.adjacency(A24,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G24)
## Match up the scores to the vector of names
centrality.h24 <- alpha.centrality(G24)
## Export the dataset
write.dta(as.data.frame(centrality.h24),"centrality_h24.dta")
plot(density.default(x=alpha.centrality(G24)))

###
###
###twentyfifth House

house.25<-readKH("ftp://voteview.com/dtaord/hou25kh.ord")
lopn25<-.025*house.25$n
lopn25
house.25<-dropUnanimous(house.25,lop=lopn25)
summary(house.25)

h.25<-house.25$votes ##Second Congress Roll Call Votes
h.25<-h.25[2:nrow(h.25),] ##Dropping President
h.25[h.25==0]<-NA ##Converting 0s to NAs
h.25<-na.omit(h.25) ##Omitting 0s aka Not in Legislature for one or more votes

legnames25<-rownames(h.25)##Names of legislators

Ano25 <- agree.mat(h.25) ##For all votes in twentyfifth House
rownames(Ano25)<-rownames(h.25)
colnames(Ano25)<-rownames(h.25)

A25 <- Ano25/ncol(h.25) ##percentage of times they agree
data25 <- as.data.frame(A25) ##First Congress agreement scores as data frame

n25 <- length(data25)

head(data25)##look at agreement scores with names listed
dimnames(data25)
# Make an empty vector
pairdata_full25 <- matrix(NA,(n25*(n25-1)/2),1)
pairdata25 <- (data25[(1+1):n25,1])
name_one25 <- matrix(1,(n25-1),1)
name_two25 <- matrix(2:n25, (n25-1),1)

# Fill in the data

for (i in 2:n25) {
  if ((i+1)<=n25){
    pairdata25 <- c(pairdata25, data25[(1+i):n25,i])
    names25 <- length(data25[(1+i):n25,i])
    name_one25 <- c(name_one25, matrix(i,names25,1))
    name_two25 <- c(name_two25, matrix((i+1):n25, names25,1))
  }
}

pairdata_full25[,1] <- t(pairdata25)

a.h25 <- as.data.frame(pairdata_full25)
colnames(a.h25) <- c("rc_agree")
write.dta(a.h25,"agreetwentyfifthhouse.dta")
head(a.h25)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G25 <- graph.adjacency(A25,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G25)
## Match up the scores to the vector of names
centrality.h25 <- alpha.centrality(G25)
## Export the dataset
write.dta(as.data.frame(centrality.h25),"centrality_h25.dta")
plot(density.default(x=alpha.centrality(G25)))

###
###
###twentysixth House

house.26<-readKH("ftp://voteview.com/dtaord/hou26kh.ord")
lopn26<-.025*house.26$n
lopn26
house.26<-dropUnanimous(house.26,lop=lopn26)
summary(house.26)

h.26<-house.26$votes ##Second Congress Roll Call Votes
h.26<-h.26[2:nrow(h.26),] ##Dropping President
h.26[h.26==0]<-NA ##Converting 0s to NAs
h.26<-na.omit(h.26) ##Omitting 0s aka Not in Legislature for one or more votes

legnames26<-rownames(h.26)##Names of legislators

Ano26 <- agree.mat(h.26) ##For all votes in twentysixth House
rownames(Ano26)<-rownames(h.26)
colnames(Ano26)<-rownames(h.26)

A26 <- Ano26/ncol(h.26) ##percentage of times they agree
data26 <- as.data.frame(A26) ##First Congress agreement scores as data frame

n26 <- length(data26)

head(data26)##look at agreement scores with names listed
dimnames(data26)
# Make an empty vector
pairdata_full26 <- matrix(NA,(n26*(n26-1)/2),1)
pairdata26 <- (data26[(1+1):n26,1])
name_one26 <- matrix(1,(n26-1),1)
name_two26 <- matrix(2:n26, (n26-1),1)

# Fill in the data

for (i in 2:n26) {
  if ((i+1)<=n26){
    pairdata26 <- c(pairdata26, data26[(1+i):n26,i])
    names26 <- length(data26[(1+i):n26,i])
    name_one26 <- c(name_one26, matrix(i,names26,1))
    name_two26 <- c(name_two26, matrix((i+1):n26, names26,1))
  }
}

pairdata_full26[,1] <- t(pairdata26)

a.h26 <- as.data.frame(pairdata_full26)
colnames(a.h26) <- c("rc_agree")
write.dta(a.h26,"agreetwentysixthhouse.dta")
head(a.h26)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G26 <- graph.adjacency(A26,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G26)
## Match up the scores to the vector of names
centrality.h26 <- alpha.centrality(G26)
## Export the dataset
write.dta(as.data.frame(centrality.h26),"centrality_h26.dta")
plot(density.default(x=alpha.centrality(G26)))

###
###
###twentyseventh House

house.27<-readKH("ftp://voteview.com/dtaord/hou27kh.ord")
lopn27<-.025*house.27$n
lopn27
house.27<-dropUnanimous(house.27,lop=lopn27)
summary(house.27)

h.27<-house.27$votes ##Second Congress Roll Call Votes
h.27<-h.27[2:nrow(h.27),] ##Dropping President
h.27[h.27==0]<-NA ##Converting 0s to NAs
h.27<-na.omit(h.27) ##Omitting 0s aka Not in Legislature for one or more votes

legnames27<-rownames(h.27)##Names of legislators

Ano27 <- agree.mat(h.27) ##For all votes in twentyseventh House
rownames(Ano27)<-rownames(h.27)
colnames(Ano27)<-rownames(h.27)

A27 <- Ano27/ncol(h.27) ##percentage of times they agree
data27 <- as.data.frame(A27) ##First Congress agreement scores as data frame

n27 <- length(data27)

head(data27)##look at agreement scores with names listed
dimnames(data27)
# Make an empty vector
pairdata_full27 <- matrix(NA,(n27*(n27-1)/2),1)
pairdata27 <- (data27[(1+1):n27,1])
name_one27 <- matrix(1,(n27-1),1)
name_two27 <- matrix(2:n27, (n27-1),1)

# Fill in the data

for (i in 2:n27) {
  if ((i+1)<=n27){
    pairdata27 <- c(pairdata27, data27[(1+i):n27,i])
    names27 <- length(data27[(1+i):n27,i])
    name_one27 <- c(name_one27, matrix(i,names27,1))
    name_two27 <- c(name_two27, matrix((i+1):n27, names27,1))
  }
}

pairdata_full27[,1] <- t(pairdata27)

a.h27 <- as.data.frame(pairdata_full27)
colnames(a.h27) <- c("rc_agree")
write.dta(a.h27,"agreetwentyseventhhouse.dta")
head(a.h27)

##ALpha Centrality

require(igraph)
## First we create a graph object from the adjacency matrix (the percentage of agreements). The "weighted" argument says that the cell entries indicate the weight of the edges.
G27 <- graph.adjacency(A27,weighted=TRUE)
## And then this will calculate and display the centrality scores
alpha.centrality(G27)
## Match up the scores to the vector of names
centrality.h27 <-alpha.centrality(G27)
## Export the dataset
write.dta(as.data.frame(centrality.h27),"centrality_h27.dta")
plot(density.default(x=alpha.centrality(G27)))


##### Comparisons between Houses ###########
a.h3.m<-as.matrix(a.h3)
a.h4.m<-as.matrix(a.h4)
(ks.h3a<-ks.test(a.h3.m,a.h4.m))##Significant.
(ks.h3c<-ks.test(centrality.h3,centrality.h4))## Not Significant

a.h13.m<-as.matrix(a.h13)
a.h14.m<-as.matrix(a.h14)

(ks.h13a<-ks.test(a.h13.m, a.h14.m)) ##Significant 
(ks.h13c<-ks.test(centrality.h13, centrality.h14)) ##Significant

house1_3<-rbind(as.matrix(a.h1), as.matrix(a.h2), as.matrix(a.h3))

house4_6<-rbind(as.matrix(a.h4), as.matrix(a.h5), as.matrix(a.h6))

h1_3.m<-as.matrix(house1_3)
h4_6.m<-as.matrix(house4_6)
ks.test(h1_3.m,h4_6.m)

hcent1_3<-c(centrality.h1, centrality.h2, centrality.h3)
hcent4_6<-c(centrality.h4, centrality.h5, centrality.h6)
ks.test(hcent1_3, hcent4_6)

##Won't run if not in matrix form, so converting. 
house10_12<-rbind(as.matrix(a.h10), as.matrix(a.h11), as.matrix(a.h12))
house13_15<-rbind(as.matrix(a.h13), as.matrix(a.h14), as.matrix(a.h15))

##KS Test for agreement scores. Sig. 
ks.test(house10_12, house13_15)

##Ks Test for centrality scores. Not Sig. 
ks.test(c(centrality.h10, centrality.h11, centrality.h12), c(centrality.h13, centrality.h14, centrality.h15))

myfun<-function(x){
  apply(x,1,as.matrix)
  y<-rbind(x)
  return(y)
}
##Making matrix form of all congress' agreement scores
h1_13<-as.matrix(rbind(a.h1, a.h2, a.h3, a.h4, a.h5, a.h6, a.h7, a.h8, a.h9, a.h10, a.h11, a.h12, a.h13))
h14_27<-as.matrix(rbind(a.h14, a.h15, a.h16, a.h17, a.h18, a.h19, a.h20, a.h21, a.h22, a.h23, a.h24, a.h25, a.h26, a.h27))

ks.test(h1_13, h14_27)

##Centrality scores

ks.test(c(centrality.h1, centrality.h2, centrality.h3, centrality.h4, centrality.h5, centrality.h6, centrality.h7, centrality.h8, centrality.h9, centrality.h10, centrality.h11, centrality.h12, centrality.h13), 
        c(centrality.h14, centrality.h15, centrality.h16, centrality.h17, centrality.h18, centrality.h19, centrality.h20, centrality.h21, centrality.h22, centrality.h23, centrality.h24, centrality.h25, centrality.h26, centrality.h27))

##########Plots #####

plot(density(a.h3.m), col="blue", main="Agreement Density in 3rd and 4th Houses")
lines(density(a.h4.m), col="red")

plot(density(centrality.h3),col="blue", main="Centrality Density in 3rd and 4th Houses")
lines(density(centrality.h4), col="red")

plot(density(a.h13.m), col="blue", main="Agreement Density in 13th and 14th Houses")
lines(density(a.h14.m), col="red")

plot(density(centrality.h13),col="blue", main="Centrality Density in 13th and 14th Houses")
lines(density(centrality.h14), col="red")

plot(density(c(centrality.h1, centrality.h2, centrality.h3, 
               centrality.h4, centrality.h5, centrality.h6, 
               centrality.h7, centrality.h8, centrality.h9, 
               centrality.h10, centrality.h11, centrality.h12, 
               centrality.h13)), col="blue", main="Centrality Houses 1-13 v 14-27")
lines(density(c(centrality.h14, centrality.h15, centrality.h16, 
                centrality.h17, centrality.h18, centrality.h19, 
                centrality.h20, centrality.h21, centrality.h22, 
                centrality.h23, centrality.h24, centrality.h25, 
                centrality.h26, centrality.h27)),col="red")
