
myhegyIII<-function(n,tt)  {
  
  ISNy1=matrix(0,1,tt)   # Intercept  seasonal no trend 
  ISNy2=matrix(0,1,tt)
  ISNy3=matrix(0,1,tt)   
  ISNy4=matrix(0,1,tt)
  ISNy3y4=matrix(0,1,tt)
  
  
  for (i in 1:tt) {
  
    e=rnorm((n+4),0,1)
    y=cbind(e[1],e[2],e[3],e[4])
    
    for (j in 5:(n+4))  { 
      
      y[j]=e[j]+y[j-4]
    }
    
    y<-zoo(y)
    l1y<-lag(y,-1,na.pad=TRUE)
    l2y<-lag(y,-2,na.pad=TRUE)
    l3y<-lag(y,-3,na.pad=TRUE)
    l4y<-lag(y,-4,na.pad=TRUE)
    # cbind(y , l1y ,l2y , l3y)
    Y1=y+l1y+l2y+l3y
    Y2=-y+l1y-l2y+l3y
    Y31=-y+l2y 
    Y32=-l1y+l3y 
    DY=y-l4y
    Y1L<-lag(Y1,-1,na.pad=TRUE)
    Y2L<-lag(Y2,-1,na.pad=TRUE)
    Y31L<-lag(Y31,-1,na.pad=TRUE)
    Y32L<-lag(Y32,-1,na.pad=TRUE)
    nn=(n+4)/4  ## creating dummies 
   
    dummy1<-rep(c(0,1,0,0)  , times=nn)
    dummy2<-rep(c(0,0,1,0)  , times=nn)
    dummy3<-rep(c(0,0,0,1)  , times=nn)
    
    
    regresion=dynlm(DY~Y1L+Y2L+Y31L+Y32L+dummy1+dummy2+dummy3 )
    mysum<-summary(regresion)
    # names(mysum)
    ISNy1[1,i]=mysum$coefficients[2,3]
    ISNy2[1,i]=mysum$coefficients[3,3]
    ISNy3[1,i]=mysum$coefficients[4,3]
    ISNy4[1,i]=mysum$coefficients[5,3]
    myF<-linearHypothesis(regresion,c("Y31L=0","Y32L=0"),test="F")
    ISNy3y4[1,i]=myF[2,5]
    
  }
  
  
  ISNy1<-sort(ISNy1)
  ISNy2<-sort(ISNy2)
  ISNy3<-sort(ISNy3)
  ISNy4<-sort(ISNy4)
  ISNy3y4<-sort(ISNy3y4)
  qISN1<-quantile(ISNy1,c(0.01 , 0.025,  0.05,  0.10))
  qISN2<-quantile(ISNy2,c(0.01 , 0.025,  0.05,  0.10))
  qISN3<-quantile(ISNy3,c(0.01 , 0.025,  0.05,  0.10))
  qISN4<-quantile(ISNy4,c(0.01 , 0.025,  0.05,  0.10))
  qISN34<-quantile(ISNy3y4,c(0.90 , 0.95,  0.975,  0.99))
  
  
  mylist<-list(qISN1,qISN2,qISN4,qISN3,qISN34)
  
  return(mylist)
}

