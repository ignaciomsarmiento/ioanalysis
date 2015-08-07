influence<-function(mip,X, col,row, write.xlsx=FALSE, name="Field_of_Influence.xlsx"){
  B<-as.matrix(leontief.inv(mip,X,FALSE))
  temp<-matrix(B[,col])%*%B[row,]
  if(write.xlsx==TRUE) write.xlsx2(temp, file=name, sheetName="Sheet1", showNA=TRUE)
  temp
}
