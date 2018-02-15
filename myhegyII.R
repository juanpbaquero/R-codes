

myhegyII<-function(n,tt)  {
  
  INNy1=matrix(0,1,tt)   # Intercept no seasonal no trend 
  INNy2=matrix(0,1,tt)
  INNy3=matrix(0,1,tt)   
  INNy4=matrix(0,1,tt)
  INNy3y4=matrix(0,1,tt)
  
  
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
    cbind(DY,Y1L,Y2L,Y31L,Y32L)
    regresion=dynlm(DY~Y1L+Y2L+Y31L+Y32L )
    mysum<-summary(regresion)
    # names(mysum)
    INNy1[1,i]=mysum$coefficients[2,3]
    INNy2[1,i]=mysum$coefficients[3,3]
    INNy3[1,i]=mysum$coefficients[4,3]
    INNy4[1,i]=mysum$coefficients[5,3]
    myF<-linearHypothesis(regresion,c("Y31L=0","Y32L=0"),test="F")
    INNy3y4[1,i]=myF[2,5]
    
  }
  
  
  INNy1<-sort(INNy1)
  INNy2<-sort(INNy2)
  INNy3<-sort(INNy3)
  INNy4<-sort(INNy4)
  INNy3y4<-sort(INNy3y4)
  qINN1<-quantile(INNy1,c(0.01 , 0.025,  0.05,  0.10))
  qINN2<-quantile(INNy2,c(0.01 , 0.025,  0.05,  0.10))
  qINN3<-quantile(INNy3,c(0.01 , 0.025,  0.05,  0.10))
  qINN4<-quantile(INNy4,c(0.01 , 0.025,  0.05,  0.10))
  qINN34<-quantile(INNy3y4,c(0.90 , 0.95,  0.975,  0.99))
  
  
  mylist<-list(qINN1,qINN2,qINN4,qINN3,qINN34)
  
  return(mylist)
}
