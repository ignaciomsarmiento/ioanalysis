upstream<-function(mip,y,x,m, write.xlsx=TRUE, name="Upstream.xlsx"){
  if(class(mip)!="data.frame")stop("Should be element of a data.frame class")
  nosect<-dim(mip)[1]
  limp<-(mip/y)
  w<-y/(y-x+m)
  dij<-mip*w
  d.inv<-solve(diag(nosect)-dij)
  u<-cbind(rep(1,nosect))
  U<-d.inv%*%u
  if(write.xlsx==TRUE)write.xlsx(U, file=name, sheetName="Sheet1", showNA=TRUE)
  U
}