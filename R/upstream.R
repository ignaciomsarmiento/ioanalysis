upstream<-function(mip,y,x,m, write.xlsx=TRUE, name="Upstream.xlsx"){
  if(class(mip)!="matrix") mip<-as.matrix(mip)
  nosect<-dim(mip)[1]
  mip<-(mip/y)
  w<-y/(y-x+m)
  dij<-mip*w
  d.inv<-solve(diag(nosect)-dij)
  u<-cbind(rep(1,nosect))
  U<-d.inv%*%u
  if(write.xlsx==TRUE)write.xlsx(U, file=name, sheetName="Sheet1", showNA=TRUE)
  U
}