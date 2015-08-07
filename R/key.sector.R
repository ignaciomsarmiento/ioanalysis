key.sector<-function(mip, X, epsilon=0.1, key=TRUE, cutoff=1, write.xlsx=FALSE, name="Key_sector.xlsx"){
  L<-leontief.inv(mip, X, linkages=TRUE, write.xlsx=FALSE, name="Leontief_Inv.xlsx")
  nosect<-dim(mip)[1]
  sector.numb<-seq(1:nosect)
  b<-data.frame(sector.numb,L[,(nosect+1)],L[,(nosect+2)])
  rownames(b)<-names(mip)
  colnames(b)<- c("Sector Number", "Backward Linkage", "Forward Linkage")
  if(write.xlsx==TRUE) write.xlsx2(b, file=name, sheetName="Sheet1", showNA=TRUE)
  if(key==TRUE){
    b$Key<-ifelse(b$Forward>=cutoff&b$Backward>=cutoff,"Key",ifelse(b$Forward>=cutoff&b$Backward<cutoff,"II",ifelse(b$Forward<cutoff&b$Backward<cutoff,"III","IV")))
    b<-b[order(-b$Backward,-b$Forward),] 
  }
  b
}



