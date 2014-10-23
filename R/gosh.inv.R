gosh.inv<-function(mip, X.i, write.xlsx=TRUE, name="Gosh_Inv.xlsx"){
  if(dim(mip)[2]!=dim(X.i)[1])stop("check dimensions")
  nosect<-length(X.i)
  for (i in 1:nosect) if (X.i[i]==0) X.i[i]=0.1
  aij=mip/X.i
  inv<-solve(diag(nosect)-aij)
  bij<-data.frame(t(inv))
  colnames(bij)<-names(mip)
  V<-sum(bij)/nosect
  b.i<-rowSums(bij)
  for (i in 1:nosect) if (b.i[i]==1)b.i[i]=0
  bij$flink<-b.i/V
  colnames(bij)[(nosect+1)]<- "Forward Linkage"
  if(write.xlsx==TRUE) write.xlsx2(bij, file=name, sheetName="Sheet1", showNA=TRUE)
  bij
}



