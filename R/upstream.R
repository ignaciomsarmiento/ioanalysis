upstream<-function(A,y,x,m, write.xlsx=TRUE, name="Upstream.xlsx"){
  if(class(A)!="matrix") A<-as.matrix(A)
  if(class(y)!="numeric") y<-as.numeric(y)
  if(class(x)!="numeric") x<-as.numeric(x)
  if(class(m)!="numeric") m<-as.numeric(m)
  nosect<-dim(A)[1]
  A<-(A/y)
  w<-y/(y-x+m)
  dij<-A*w
  d.inv<-solve(diag(nosect)-dij)
  u<-cbind(rep(1,nosect))
  U<-d.inv%*%u
  if(write.xlsx==TRUE)write.xlsx(U, file=name, sheetName="Sheet1", showNA=TRUE)
  U
}