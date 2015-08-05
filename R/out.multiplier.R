multipliers<-function(mip, X, z, e,write.xlsx=TRUE, name="output_multiplier.xlsx"){
  if(missing(z)&missing(e)){
    B<-leontief.inv(mip, X, write.xlsx=FALSE)
    A1<-as.matrix(colSums(B))
    C<-gosh.inv(mip, X, write.xlsx=FALSE)
    A2<-as.matrix(colSums(C))
    A<-cbind(A1,A2)
    A<-A[!(rownames(A)=="Backward Linkage"),]
    A<-as.matrix(A)
    colnames(A)<- c("Output Multiplier", "Input Multiplier")
    if(write.xlsx==TRUE) write.xlsx2(A, file=name, sheetName="Sheet1", showNA=TRUE)
    A<-as.data.frame(A)  
    A
  }
  if(missing(e)){
    if(length(z)!=dim(mip)[1])stop("Check dimensions/length I/O matrix and household input coefficient.")
    if(length(z)!=length(X))stop("Check lengths output vector and household input coefficient.")
    a<-z/X
    a<-as.matrix(a)
    a<-t(a)
    B<-leontief.inv(mip, X, write.xlsx=FALSE)
    B<-B[,!(colnames(B)=="Backward Linkage")]
    if(dim(a)[2]!=dim(B)[1])("Check dimensions z, X and MIP")
    class(B)
    class(a)
    H<-a%*%B
    H<-t(H)
    A1<-as.matrix(colSums(B))
    C<-gosh.inv(mip, X, write.xlsx=FALSE)
    A2<-as.matrix(colSums(C))
    A<-cbind(A1,A2,H)
    A<-A[!(rownames(A)=="Backward Linkage"),]
    A<-as.matrix(A)
    colnames(A)<- c("Output Multiplier", "Input Multiplier", "Income Multiplier")
    if(write.xlsx==TRUE) write.xlsx2(A, file=name, sheetName="Sheet1", showNA=TRUE)
    A<-as.data.frame(A)  
    A
  }
  
}

data<-load("data/mat_40x40.Rda")
mip<-mat_40x40[1:40,2:41] #Input-output coeffcients
X<-as.matrix(mat_40x40$DT.a.PB[1:40])  #Total output vector
z<-mat_40x40$X[1:40]

multipliers<-multipliers(mip,X,z, write.xlsx=TRUE)




#Clean out the workspace
#dev.off()
rm(list=ls())
cat("\014") 
source("R/leontief.inv.R")
source("R/gosh.inv.R")
list.files()