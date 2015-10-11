key.sector<-function(mip, X, dietz=FALSE, key=TRUE, cutoff=1, write.xlsx=FALSE, name="Key_sector.xlsx"){
  nosect<-dim(mip)[1]
  sector.numb<-seq(1:nosect)
  if(dietz==TRUE){
    L<-leontief.inv(mip, X, linkages=FALSE, write.xlsx=FALSE)
    n<-dim(L)[1]
    ones<-rep(1,n)
    e<-eigen(L)
    rev<-e$vectors[,which.max(Re(e$values))]
    FL_die<-Re(n*rev/(t(ones)%*%rev))
    #Backward Linkages
    e<-eigen(t(L))
    lev<-e$vectors[,which.max(Re(e$values))]
    BL_die<-Re(n*lev/(lev%*%ones))
    b<-data.frame(sector.numb,BL_die,FL_die)
  } 
  else {
    L<-leontief.inv(mip, X, linkages=TRUE, write.xlsx=FALSE, name="Leontief_Inv.xlsx")
    b<-data.frame(sector.numb,L[,(nosect+1)],L[,(nosect+2)])
    }
  rownames(b)<-names(mip)
  colnames(b)<- c("Sector Number", "Backward Linkage", "Forward Linkage")
  if(write.xlsx==TRUE) write.xlsx2(b, file=name, sheetName="Sheet1", showNA=TRUE)
  if(key==TRUE){
    b$Key<-ifelse(b$Forward>=cutoff&b$Backward>=cutoff,"Key",ifelse(b$Forward>=cutoff&b$Backward<cutoff,"II",ifelse(b$Forward<cutoff&b$Backward<cutoff,"III","IV")))
    b<-b[order(-b$Backward,-b$Forward),] 
  }
  b
}



