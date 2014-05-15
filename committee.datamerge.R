setwd("C:/Users/emily m/Journal Articles/Committees and Networks/House committees")
library(foreign)

##Bind together the houses with the same census data
theHouses<-rbind(house1, house2, house3, house4, house5, house6)
theHouses2<-rbind(house7, house8, house9, house10, house11)
theHouses3<-rbind(house12, house13, house14, house15, house16)
theHouses4<-rbind(house17, house18, house19, house20, house21)
theHouses5<-rbind(house22, house23, house24, house25, house26)
dim(theHouses)+dim(theHouses2)+dim(theHouses3)+dim(theHouses4)+dim(theHouses5)+dim(house27)

##Renames some columns for compatibility.
colnames(theHouses)[4]<-"icpsr"
colnames(theHouses)[9]<-"urbyr"
colnames(theHouses2)<-colnames(theHouses)
colnames(theHouses3)<-colnames(theHouses)

##Remove some unnecessary cols.
theHouses4<-(theHouses4[,-(18:20)])

##Add together smtot and sftot to get the total slave pop.
theHouses4$stot<-theHouses4$smtot+theHouses4$sftot
head(theHouses4)
##Get rid of old smtot and sftot
theHouses4<-theHouses4[,-(11:12)]

##Rename as necessary
colnames(theHouses4)[4]<-"icpsr"
colnames(theHouses4)[9]<-"urbyr"
head(theHouses4)
head(theHouses2)
head(theHouses)
##Bind together the houses which are now compatible.
theHouses<-rbind(theHouses, theHouses2, theHouses3)
colnames(theHouses)
##get rid of other free persons as other years don't have this
theHouses<-theHouses[,-(11)]

##bind together through theHouses4
theHouses<-(rbind(theHouses, theHouses4[colnames(theHouses)]))

colnames(theHouses5)<-colnames(house27)<-colnames(theHouses)

theHouses<-rbind(theHouses, theHouses5, house27)

thesenates<-rbind(senate1, senate2, senate3, senate4, senate5, senate6)
thesenates2<-rbind(senate7, senate8, senate9, senate10, senate11)
thesenates3<-rbind(senate12, senate13, senate14, senate15, senate16)
thesenates4<-rbind(senate17, senate18, senate19, senate20, senate21)
thesenates5<-rbind(senate22, senate23, senate24, senate25, senate26)
dim(thesenates)+dim(thesenates2)+dim(thesenates3)+dim(thesenates4)+dim(thesenates5)+dim(senate27)

##Renames some columns for compatibility.
colnames(thesenates)[4]<-"icpsr"
colnames(thesenates)[9]<-"urbyr"
colnames(thesenates2)<-colnames(thesenates)
colnames(thesenates3)<-colnames(thesenates)

##Remove some unnecessary cols.
thesenates4<-(thesenates4[,-(18:20)])

##Add together smtot and sftot to get the total slave pop.
thesenates4$stot<-thesenates4$smtot+thesenates4$sftot
head(thesenates4)
##Get rid of old smtot and sftot
thesenates4<-thesenates4[,-(11:12)]

##Rename as necessary
colnames(thesenates4)[4]<-"icpsr"
colnames(thesenates4)[9]<-"urbyr"
head(thesenates4)
head(thesenates2)
head(thesenates)
##Bind together the senates which are now compatible.
thesenates<-rbind(thesenates, thesenates2, thesenates3)
colnames(thesenates)
##get rid of other free persons as other years don't have this
thesenates<-thesenates[,-(11)]

##bind together through thesenates4
thesenates<-(rbind(thesenates, thesenates4[colnames(thesenates)]))

colnames(thesenates5)<-colnames(senate27)<-colnames(thesenates)

thesenates<-rbind(thesenates, thesenates5, senate27)

thesenates$congressnum

library(foreign)
write.dta(thesenates, "SenateMemberInfo_cong1thru27.dta")

(read.dta("SenateMemberInfo_cong1thru27.dta")[500:1000,]
 )
