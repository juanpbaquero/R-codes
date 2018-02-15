
setwd("C:\\Users\\Juan Pablo\\Documents\\R")
source("myhegyI.R")
source("myhegyII.R")
source("myhegyIII.R")
source("myhegyIIII.R")
source("myhegyV.R")
require(car)
require(dynlm)
require(zoo)
sam=c(48,100,136,200)
TT=30000;
# no intercept no seasonal no trend 

matrixy1=matrix(0,4,4)
matrixy2=matrix(0,4,4)
matrixy3=matrix(0,4,4)
matrixy4=matrix(0,4,4)
matrixy34=matrix(0,4,4)

#  intercept no seasonal no trend 

matrixy1I=matrix(0,4,4)
matrixy2I=matrix(0,4,4)
matrixy3I=matrix(0,4,4)
matrixy4I=matrix(0,4,4)
matrixy34I=matrix(0,4,4)

#  intercept seasonal no trend 

matrixy1II=matrix(0,4,4)
matrixy2II=matrix(0,4,4)
matrixy3II=matrix(0,4,4)
matrixy4II=matrix(0,4,4)
matrixy34II=matrix(0,4,4)

# intercept no seasonal trend 

matrixy1III=matrix(0,4,4)
matrixy2III=matrix(0,4,4)
matrixy3III=matrix(0,4,4)
matrixy4III=matrix(0,4,4)
matrixy34III=matrix(0,4,4)

# intercept seasonal trend 

matrixy1IIII=matrix(0,4,4)
matrixy2IIII=matrix(0,4,4)
matrixy3IIII=matrix(0,4,4)
matrixy4IIII=matrix(0,4,4)
matrixy34IIII=matrix(0,4,4)





for (i in 1:4) {

mylist<-myhegyI(sam[i],TT)    # no intercept no seasonal no trend 
mylist <- matrix(unlist(mylist), ncol =4, byrow = TRUE)

matrixy1[i,]=mylist[1,]
matrixy2[i,]=mylist[2,]
matrixy3[i,]=mylist[3,]
matrixy4[i,]=mylist[4,]
matrixy34[i,]=mylist[5,]

mylist2<-myhegyII(sam[i],TT)    #  intercept no seasonal no trend 
mylist2 <- matrix(unlist(mylist2), ncol =4, byrow = TRUE)

matrixy1I[i,]=mylist2[1,]
matrixy2I[i,]=mylist2[2,]
matrixy3I[i,]=mylist2[3,]
matrixy4I[i,]=mylist2[4,]
matrixy34I[i,]=mylist2[5,]

mylist3<-myhegyIII(sam[i],TT)    #  intercept  seasonal no trend 
mylist3<- matrix(unlist(mylist3), ncol =4, byrow = TRUE)

matrixy1II[i,]=mylist3[1,]
matrixy2II[i,]=mylist3[2,]
matrixy3II[i,]=mylist3[3,]
matrixy4II[i,]=mylist3[4,]
matrixy34II[i,]=mylist3[5,]

mylist4<-myhegyIIII(sam[i],TT)    #  intercept no seasonal trend 
mylist4<- matrix(unlist(mylist4), ncol =4, byrow = TRUE)

matrixy1III[i,]=mylist4[1,]
matrixy2III[i,]=mylist4[2,]
matrixy3III[i,]=mylist4[3,]
matrixy4III[i,]=mylist4[4,]
matrixy34III[i,]=mylist4[5,]

mylist5<-myhegyV(sam[i],TT)    #  intercept seasonal trend 
mylist5<- matrix(unlist(mylist5), ncol =4, byrow = TRUE)

matrixy1IIII[i,]=mylist5[1,]
matrixy2IIII[i,]=mylist5[2,]
matrixy3IIII[i,]=mylist5[3,]
matrixy4IIII[i,]=mylist5[4,]
matrixy34IIII[i,]=mylist5[5,]


}




