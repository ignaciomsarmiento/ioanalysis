vs.io.leon<-function(imp,exp,leon,namesector, write.xlsx=TRUE, name="Level_Verticalization.xlsx"){
  if(dim(imp)[2]!=dim(exp)[1])stop("check dimensions")
  ax<-as.matrix(imp)%*%as.matrix(leon)%*%exp
  xk<-sum(exp)
  vert<-ax/xk
  df <-data.frame(sectors = c(namesector),  vs=c(ax), exports=c(exp), level_verticalization = c(vert)) 
  format(df, digits=2, nsmall=2, scientific = FALSE)
  if(write.xlsx==TRUE)write.xlsx(df, file=name, sheetName="Sheet1", showNA=TRUE)
  list(format(df, digits=2, nsmall=2, scientific = FALSE), total.exp=(format(sum(exp), scientific = FALSE)))
}

