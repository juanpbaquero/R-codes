

myhegy<-function(n,tt)  {

NNNy1=matrix(0,1,tt)   # no seasonal no intercept no trend
NNNy2=matrix(0,1,tt)
NNNy3=matrix(0,1,tt)   # no seasonal no intercept no trend
NNNy4=matrix(0,1,tt)

for (i in 1:tt) {

require(dynlm)
require(zoo)
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
regresion=dynlm(DY~Y1L+Y2L+Y31L+Y32L -1)
mysum<-summary(regresion)
# names(mysum)
NNNy1[1,i]=mysum$coefficients[1,3]
NNNy2[1,i]=mysum$coefficients[2,3]
NNNy3[1,i]=mysum$coefficients[3,3]
NNNy4[1,i]=mysum$coefficients[4,3]

}


NNNy1<-sort(NNNy1)
NNNy2<-sort(NNNy2)
NNNy3<-sort(NNNy3)
NNNy4<-sort(NNNy4)
qnnn1<-quantile(NNNy1,c(0.01 , 0.025,  0.05,  0.10))
qnnn2<-quantile(NNNy2,c(0.01 , 0.025,  0.05,  0.10))
qnnn3<-quantile(NNNy3,c(0.01 , 0.025,  0.05,  0.10))
qnnn4<-quantile(NNNy4,c(0.01 , 0.025,  0.05,  0.10))
mylist<-list(qnnn1,qnnn2,qnnn3,qnnn4)
return(mylist)
}





