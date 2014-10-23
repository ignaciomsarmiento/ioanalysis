leontief.inv<-function(mip, X.j, write.xlsx=TRUE, name="Leontief_Inv.xlsx"){
  #Leontief inverse (domestic) is defined as
  #(I-Ad)^{-1} 
  #where Ad is the input coefficient matrix of domestic transactions. 
  #I is a diagonal matrix
  if(dim(mip)[2]!=dim(X.j)[1])stop("check dimensions")
  nosect<-length(X.j)
  for (i in 1:nosect) if (X.j[i]==0) X.j[i]=0.1
  aij=mip/X.j
  bij<-data.frame(solve(diag(nosect)-aij))
  colnames(bij)<-names(mip)
  V<-sum(bij)/nosect
  b.j<-rowSums(bij)
  for (i in 1:nosect) if (b.j[i]==1)b.j[i]=0
  bij$blink<-b.j/V
  colnames(bij)[(nosect+1)]<- "Backward Linkage"
  if(write.xlsx==TRUE) write.xlsx2(bij, file=name, sheetName="Sheet1", showNA=TRUE)
  bij
}



