upstream<-function(linv,y,x,m, write.xlsx=TRUE, name="Upstream.xlsx"){
  if(class(linv)!="data.frame")stop("Should be element of a data.frame class")
  nosect<-dim(linv)[1]
  w<-y/(y-x+m)
  dij<-linv*w
  d.inv<-solve(diag(nosect)-dij)
  u<-cbind(rep(1,nosect))
  U<-d.inv%*%u
  if(write.xlsx==TRUE)write.xlsx(U, file=name, sheetName="Sheet1", showNA=TRUE)
  U
}