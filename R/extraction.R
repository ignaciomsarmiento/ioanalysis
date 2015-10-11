extraction<-function(mip, X, f, v, forward=FALSE, write.xlsx=FALSE, name="Extraction.xlsx"){
  
  #Helper Functions
  insertRow <- function(existingDF, newrow, r) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    existingDF
  }
  
  insertCol <- function(existingDF, newcol, c) {
    existingDF[,seq(c+1,ncol(existingDF)+1)] <- existingDF[,seq(c,ncol(existingDF))]
    existingDF[,c] <- newcol
    existingDF
  }
  ####################
  
  if(forward==FALSE){
  L<-as.matrix(leontief.inv(mip,X,FALSE))
  nosect<-dim(mip)[1]
  aij<-t(t(mip) / X)
  
  z<-matrix(0,ncol=nosect,nrow=nosect)
  for(s in 1:(nosect-1)){
#     ARR<-solve(diag(dim(aij[-s,-s])[1])-aij[-s,-s])
#     ARR<-as.data.frame(ARR)
    ARR<-leontief.inv(mip[-s,-s],X[-s],FALSE)
    newrow <- rep(0,length=nosect-1)
    A<-insertRow(ARR,newrow,s)
    newcol <- rep(0,length=nosect)
    A<-insertCol(A,newcol,s)
    A<-as.matrix(A)
    A[s,s]<-1/(1-aij[s,s])
    z[,s]<-(L-A)%*%f
    z
  }
  #For the last sector
  s<-max(nosect)
  #ARR<-solve(diag(dim(aij[-s,-s])[1])-aij[-s,-s])
  #ARR<-as.data.frame(ARR)
  ARR<-leontief.inv(mip[-s,-s],X[-s],FALSE)
  newrow <- rep(0,length=nosect-1)
  A<-rbind(ARR,newrow)
  newcol <- rep(0,length=nosect)
  A<-cbind(A,newcol)
  A<-as.matrix(A)
  A[s,s]<-1/(1-aij[s,s])
  z[,s]<-(L-A)%*%f
  if(write.xlsx==TRUE) write.xlsx2(z, file=name, sheetName="Backward Linkage", showNA=TRUE)
  z
  }
  else if(forward==TRUE){
    L<-as.matrix(gosh.inv(mip,X,FALSE))
    nosect<-dim(mip)[1]
    
    z<-matrix(0,ncol=nosect,nrow=nosect)
    for(s in 1:(nosect-1)){
      ARR<-gosh.inv(mip[-s,-s],X[-s],FALSE)
      newrow <- rep(0,length=nosect-1)
      A<-insertRow(ARR,newrow,s)
      newcol <- rep(0,length=nosect)
      A<-insertCol(A,newcol,s)
      A<-as.matrix(A)
      A[s,s]<-1/(1-(mip[s,s]/X[s]))
      z[,s]<-(L-A)%*%v
      z
    }
    #For the last sector
    s<-max(nosect)
    ARR<-gosh.inv(mip[-s,-s],X[-s],FALSE)
    newrow <- rep(0,length=nosect-1)
    A<-rbind(ARR,newrow)
    newcol <- rep(0,length=nosect)
    A<-cbind(A,newcol)
    A<-as.matrix(A)
    A[s,s]<-1/(1-(mip[s,s]/X[s]))
    z[,s]<-v%*%(L-A)
    if(write.xlsx==TRUE) write.xlsx2(z, file=name, sheetName="Forward Linkage", showNA=TRUE)
    z
  }
}