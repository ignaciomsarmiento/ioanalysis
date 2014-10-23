verticalization<-function(IMP,X,E,namesector, write.xlsx=TRUE){
  vert=(IM/X)*E
  df <- data.frame(sectors = factor(namesector),  level_verticalization = c(vert)) 
  if(write.xlsx==TRUE)write.xlsx(df, file="Level_Verticalization.xlsx", sheetName="Sheet1", showNA=TRUE)
  df
}

vert.matrix<-function(IMP,X, write.xlsx=TRUE){
  if(class(IMP)!="matrix")stop("Should be element of a matrix class")
  if(class(X)!="matrix")stop("Should be element of a matrix class")

  nosect<-dim(X)[1]
  u<-rbind(rep(1,nosect))
  x.i<-cbind(apply(X,1,sum))
  vert<-(t(u %*% A %*% X))/x.i 
  if(write.xlsx==TRUE)write.xlsx(vert, file="Level_Verticalization.xlsx", sheetName="Sheet1", showNA=TRUE)
  vert
}