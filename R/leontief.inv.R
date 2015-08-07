leontief.inv<-function(mip, X, linkages=TRUE, write.xlsx=FALSE, name="Leontief_Inv.xlsx"){
  #Leontief inverse (domestic) is defined as
  #(I-Ad)^{-1} 
  #where Ad is the input coefficient matrix of domestic transactions. 
  #I is a diagonal matrix
  #X total output
  if(dim(mip)[1]!=length(X))stop("Check dimensions/length, mip should be matrix, X a vector")
  nosect<-dim(mip)[1]
  for (i in 1:nosect) if (X[i]==0) X[i]=0.1
  aij<-t(t(mip) / X)
  bij<-data.frame(solve(diag(nosect)-aij))
  colnames(bij)<-names(mip)
  V<-sum(bij)/nosect
  b.j<-colSums(bij)
  b.i<-rowSums(bij)
  for (i in 1:nosect) if (b.j[i]==1)b.j[i]=0
  if(linkages==TRUE){ bij$blink<-b.j/V
  bij$flink<-b.i/V
  colnames(bij)[(nosect+1)]<- "Backward Linkage"
  colnames(bij)[(nosect+2)]<- "Forward Linkage"
  }
  if(write.xlsx==TRUE) write.xlsx2(bij, file=name, sheetName="Sheet1", showNA=TRUE)
  bij
}



