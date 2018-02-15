

myhegyI<-function(n,tt)  {

  
# aca se crean las matrices donde se van a guardar los coeficientes   
NNNy1=matrix(0,1,tt)   # no seasonal no intercept no trend
NNNy2=matrix(0,1,tt)
NNNy3=matrix(0,1,tt)   
NNNy4=matrix(0,1,tt)
NNNy3y4=matrix(0,1,tt)

for (i in 1:tt) {
# aca creo el ruido blanco 
e=rnorm((n+4),0,1)
# aca creo el y ,  los primeros 4 valores son ruido blanco 
y=cbind(e[1],e[2],e[3],e[4])

# de 5 en adelante se define el proceso recursivamente 
for (j in 5:(n+4))  { 

y[j]=e[j]+y[j-4]
       }

# se define y como una serie de tiempo
y<-zoo(y)

# creo los rezagos de y 
l1y<-lag(y,-1,na.pad=TRUE)
l2y<-lag(y,-2,na.pad=TRUE)
l3y<-lag(y,-3,na.pad=TRUE)
l4y<-lag(y,-4,na.pad=TRUE)
# cbind(y , l1y ,l2y , l3y)

# creo las variables de hegy 
Y1=y+l1y+l2y+l3y
Y2=-y+l1y-l2y+l3y
Y31=-y+l2y 
Y32=-l1y+l3y 
DY=y-l4y

# las rezago un periodo como en el paper 

Y1L<-lag(Y1,-1,na.pad=TRUE)
Y2L<-lag(Y2,-1,na.pad=TRUE)
Y31L<-lag(Y31,-1,na.pad=TRUE)
Y32L<-lag(Y32,-1,na.pad=TRUE)

# hago la regresion 
regresion=dynlm(DY~Y1L+Y2L+Y31L+Y32L -1)
mysum<-summary(regresion)
# names(mysum)

# tomo los coeficientes t 

NNNy1[1,i]=mysum$coefficients[1,3]
NNNy2[1,i]=mysum$coefficients[2,3]
NNNy3[1,i]=mysum$coefficients[3,3]
NNNy4[1,i]=mysum$coefficients[4,3]

# tomo los coeficientes de la prueba F 

myF<-linearHypothesis(regresion,c("Y31L=0","Y32L=0"),test="F")
NNNy3y4[1,i]=myF[2,5]


## trial :  aca compruebo que la regresion sin usar las librerias de R da igual a no usarlas

# l1y<-matrix(NaN,1,(n+4))
# l2y<-matrix(NaN,1,(n+4))
# l3y<-matrix(NaN,1,(n+4))
# l4y<-matrix(NaN,1,(n+4))
# 
# l1y[2:(n+4)]=y[1:(n+4-1)]
# l2y[3:(n+4)]=y[1:(n+4-2)]
# l3y[4:(n+4)]=y[1:(n+4-3)]
# l4y[5:(n+4)]=y[1:(n+4-4)]
# 
# Y1=y+l1y+l2y+l3y
# Y2=-y+l1y-l2y+l3y
# Y31=-y+l2y 
# Y32=-l1y+l3y 
# DY=y-l4y
# 
# Y1L<-matrix(NaN,1,(n+4))
# Y2L<-matrix(NaN,1,(n+4))
# Y31L<-matrix(NaN,1,(n+4))
# Y32L<-matrix(NaN,1,(n+4))
# 
# Y1L[2:(n+4)]=Y1[1:(n+4-1)]
# Y2L[2:(n+4)]=Y2[1:(n+4-1)]
# Y31L[2:(n+4)]=Y31[1:(n+4-1)]
# Y32L[2:(n+4)]=Y32[1:(n+4-1)]
# 
# DY<-DY[-(1:4)]
# Y1L<-Y1L[-(1:4)]
# Y2L<-Y2L[-(1:4)]
# Y31L<-Y31L[-(1:4)]
# Y32L<-Y32L[-(1:4)]
# 
# regresion=lm(DY~Y1L+Y2L+Y31L+Y32L -1)
# mysum<-summary(regresion)



}


# organizo todas las simulaciones de menor a mayor 
NNNy1<-sort(NNNy1)
NNNy2<-sort(NNNy2)
NNNy3<-sort(NNNy3)
NNNy4<-sort(NNNy4)
NNNy3y4<-sort(NNNy3y4)

# tomo los percentiles 1 2.5 5 y 10 
qnnn1<-quantile(NNNy1,c(0.01 , 0.025,  0.05,  0.10))
qnnn2<-quantile(NNNy2,c(0.01 , 0.025,  0.05,  0.10))
qnnn3<-quantile(NNNy3,c(0.01 , 0.025,  0.05,  0.10))
qnnn4<-quantile(NNNy4,c(0.01 , 0.025,  0.05,  0.10))
qnnn34<-quantile(NNNy3y4,c(0.90 , 0.95,  0.975,  0.99))


mylist<-list(qnnn1,qnnn2,qnnn4,qnnn3,qnnn34)

return(mylist)
}






