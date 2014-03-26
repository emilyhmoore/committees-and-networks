
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
