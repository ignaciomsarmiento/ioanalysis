gosh.inv<-function(mip, X, write.xlsx=FALSE, name="Gosh_Inv.xlsx"){
  if(dim(mip)[1]!=length(X))stop("Check dimensions/length, mip should be matrix, X a vector")
  nosect<-dim(mip)[1]
  for (i in 1:nosect) if (X[i]==0) X[i]=0.1
  aij<-mip / X
  inv<-solve(diag(nosect)-aij)
  bij<-data.frame(t(inv))
  colnames(bij)<-names(mip)
  V<-sum(bij)/nosect
  b.i<-colSums(bij)
  for (i in 1:nosect) if (b.i[i]==1)b.i[i]=0
  if(write.xlsx==TRUE) write.xlsx2(bij, file=name, sheetName="Sheet1", showNA=TRUE)
  bij
}


