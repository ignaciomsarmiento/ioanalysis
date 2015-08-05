key.sector<-function(mip, X, epsilon=0.1, key=TRUE, cutoff=1, write.xlsx=TRUE, name="Key_sector.xlsx"){
  if(class(mip)!="matrix") mip<-as.matrix(mip)
  if(class(X)!="numeric") X<-as.vector(X)
  if( length(X) != nrow(mip)) X<-t(X)
  nosect<-length(X)
  for (i in 1:nosect) if(X[i]==0) X[i]=epsilon
  aij=mip/X
  bij_leon<-data.frame(solve(diag(nosect)-aij))
  V<-sum(bij_leon)/nosect
  b.j<-rowSums(bij_leon)
  for (i in 1:nosect) if(b.j[i]==1) b.j[i]<-0
  blink<-b.j/V
  bij_gosh<-data.frame(t(bij_leon))
  b.i<-rowSums(bij_gosh)
  for (i in 1:nosect) if (b.i[i]==1)b.i[i]<-0
  flink<-b.i/V
  sector.numb<-seq(1:nosect)
  b<-data.frame(sector.numb,blink,flink)
  rownames(b)<-names(mip)
  colnames(b)<- c("Sector Number", "Backward Linkage", "Forward Linkage")
  if(write.xlsx==TRUE) write.xlsx2(b, file=name, sheetName="Sheet1", showNA=TRUE)
  if(key==TRUE){
    b$key<-ifelse(b$Forward>=cutoff&b$Backward>=cutoff,"Key",ifelse(b$Forward>=cutoff&b$Backward<cutoff,"II",ifelse(b$Forward<cutoff&b$Backward<cutoff,"III","IV")))
    b<-b[order(-b$Forward,-b$Backward),] 
  }
  b
}
