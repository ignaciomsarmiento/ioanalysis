"output.decomposition"<-function(mip0, mip1,X0, X1,f0,f1, write.xlsx=FALSE, name="Output_Decomposition.xlsx"){
    if(dim(mip0)[1]!=length(X0))stop("Check dimensions/length, mip should be matrix, X a vector")
    if(dim(mip1)[1]!=length(X1))stop("Check dimensions/length, mip should be matrix, X a vector")
  
    B0<-as.matrix(leontief.inv(mip0,X0,FALSE))
    B1<-as.matrix(leontief.inv(mip1,X1,FALSE))
    decom_1<-B0%*%as.matrix(f1-f0)
    decom_2<-(B1-B0)%*%(as.matrix(f0))
    decom_3<-(B1-B0)%*%(as.matrix(f1-f0))
    self_Ch_1<-diag(B0)*(as.matrix(f1-f0))
    self_Ch_2<-(diag(B1)-diag(B0))*(as.matrix(f0))
    self_Ch_3<-(diag(B1)-diag(B0))*(as.matrix(f1-f0))
    non_self_Ch_1<-decom_1-self_Ch_1
    non_self_Ch_2<-decom_2-self_Ch_2
    non_self_Ch_3<-decom_3-self_Ch_3
    A<-cbind(decom_1,decom_2,decom_3,self_Ch_1,self_Ch_2,self_Ch_3,non_self_Ch_1,non_self_Ch_2,non_self_Ch_3)
    colnames(A)<-c("Decom_1","Decom_2","Decom_3","Self_Ch_1","Self_Ch_2","Self_Ch_3","Non_Self_Ch_1","Non_Self_Ch_2","Non_Self_Ch_3")
    if(write.xlsx==TRUE) write.xlsx2(A, file=name, sheetName="Sheet1", showNA=TRUE)
    A
}