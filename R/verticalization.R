verticalization<-function(IMP,X,E,namesector, write.xlsx=TRUE){
  vert=(IM/X)*E
  df <- data.frame(sectors = factor(namesector),  level_verticalization = c(vert)) 
  if(write.xlsx==TRUE)write.xlsx(df, file="Level_Verticalization.xlsx", sheetName="Sheet1", showNA=TRUE)
  df
}

vert.matrix<-function(IMP,X, write.xlsx=TRUE, name="Level_Verticalization.xlsx"){
  if(class(IMP)!="matrix")stop("Should be element of a matrix class")
  if(class(X)!="matrix")stop("Should be element of a matrix class")
  nosect<-dim(X)[1]
  u<-rbind(rep(1,nosect))
  x.i<-rowSums(X)
  vert<-(u %*%t(A) %*% X)/x.i 
  if(write.xlsx==TRUE)write.xlsx(vert, file=name, sheetName="Sheet1", showNA=TRUE)
  t(vert)
}

vert.matrix.leon<-function(IMP,X,L, write.xlsx=TRUE, name="Level_Verticalization.xlsx"){
  if(class(IMP)!="matrix")stop("Should be element of a matrix class")
  if(class(X)!="matrix")stop("Should be element of a matrix class")
  nosect<-dim(X)[1]
  u<-rbind(rep(1,nosect))
  x.i<-rowSums(X)
  vert<-(u %*%t(A)%*%L%*% X)/x.i 
  if(write.xlsx==TRUE)write.xlsx(vert, file=name, sheetName="Sheet1", showNA=TRUE)
  t(vert)
}
