multipliers<-function(mip, X,z,e, write.xlsx=FALSE, name="output_multiplier.xlsx"){
  if(missing(z)&missing(e)){
    B<-leontief.inv(mip, X, linkages=FALSE,write.xlsx=FALSE)
    A1<-as.matrix(colSums(B))
    C<-gosh.inv(mip, X, write.xlsx=FALSE)
    A2<-as.matrix(colSums(C))
    A<-cbind(A1,A2)
    A<-as.matrix(A)
    colnames(A)<- c("Output Multiplier", "Input Multiplier")
    if(write.xlsx==TRUE) write.xlsx2(A, file=name, sheetName="Sheet1", showNA=TRUE)
    A<-as.data.frame(A)  
    A
  }
  else if(!missing(z)&missing(e)){
    if(length(z)!=dim(mip)[1])stop("Check dimensions/length I/O matrix and household input coefficient.")
    if(length(z)!=length(X))stop("Check lengths output vector and household input coefficient.")
    a<-z/X
    a<-as.matrix(a)
    a<-t(a)
    B<-leontief.inv(mip, X, linkages=FALSE,write.xlsx=FALSE)
    B<-as.matrix(B)
    if(dim(a)[2]!=dim(B)[1])("Check dimensions z, X and MIP")
    H<-a%*%B
    H<-t(H)
    A1<-as.matrix(colSums(B))
    C<-gosh.inv(mip, X, write.xlsx=FALSE)
    A2<-as.matrix(colSums(C))
    A<-cbind(A1,A2,H)
    A<-as.matrix(A)
    colnames(A)<- c("Output Multiplier", "Input Multiplier", "Income Multiplier")
    if(write.xlsx==TRUE) write.xlsx2(A, file=name, sheetName="Sheet1", showNA=TRUE)
    A<-as.data.frame(A)  
    A
  }
  else if(missing(z)&!missing(e)){
    if(length(e)!=dim(mip)[1])stop("Check dimensions/length I/O matrix and household input coefficient.")
    if(length(e)!=length(X))stop("Check lengths output vector and household input coefficient.")
    w<-e/X
    w<-as.matrix(w)
    w<-t(w)
    B<-leontief.inv(mip, X, linkages=FALSE,write.xlsx=FALSE)
    B<-B[,!(colnames(B)=="Backward Linkage")]
    B<-as.matrix(B)
    if(dim(w)[2]!=dim(B)[1])("Check dimensions w, X and MIP")
    E<-w%*%B
    E<-t(E)
    A1<-as.matrix(colSums(B))
    C<-gosh.inv(mip, X, write.xlsx=FALSE)
    A2<-as.matrix(colSums(C))
    A<-cbind(A1,A2,E)
    A<-as.matrix(A)
    colnames(A)<- c("Output Multiplier", "Input Multiplier", "Employment Multiplier")
    if(write.xlsx==TRUE) write.xlsx2(A, file=name, sheetName="Sheet1", showNA=TRUE)
    A<-as.data.frame(A)  
    A
  }
  else {
  if(length(z)!=dim(mip)[1])stop("Check dimensions/length I/O matrix and household input coefficient.")
  if(length(z)!=length(X))stop("Check lengths output vector and household input coefficient.")
  if(length(e)!=dim(mip)[1])stop("Check dimensions/length I/O matrix and household input coefficient.")
  if(length(e)!=length(X))stop("Check lengths output vector and household input coefficient.")
  w<-e/X
  w<-as.matrix(w)
  w<-t(w)
  B<-leontief.inv(mip, X, linkages=FALSE,write.xlsx=FALSE)
  B<-as.matrix(B)
  if(dim(w)[2]!=dim(B)[1])("Check dimensions w, X and MIP")
  E<-w%*%B
  E<-t(E)
  a<-z/X
  a<-as.matrix(a)
  a<-t(a)
  if(dim(a)[2]!=dim(B)[1])("Check dimensions z, X and MIP")
  H<-a%*%B
  H<-t(H)
  A1<-as.matrix(colSums(B))
  C<-gosh.inv(mip, X, write.xlsx=FALSE)
  A2<-as.matrix(colSums(C))
  A<-cbind(A1,A2,H,E)
  A<-as.matrix(A)
  colnames(A)<- c("Output Multiplier", "Input Multiplier", "Income Multiplier","Employment Multiplier")
  if(write.xlsx==TRUE) write.xlsx2(A, file=name, sheetName="Sheet1", showNA=TRUE)
  A<-as.data.frame(A)  
  A
  }
}



