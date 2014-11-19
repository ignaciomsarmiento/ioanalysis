vs.io<-function(imp,exp,namesector,leon=1, write.xlsx=TRUE, name="Level_Verticalization.xlsx"){
  if(length(leon)==1){ax<-as.matrix(imp)%*%as.matrix(exp)}
  else{ax<-as.matrix(imp)%*%as.matrix(leon)%*%as.matrix(exp)}
  xk<-sum(exp)
  vert<-ax/xk
  df <-data.frame(sectors = c(namesector),  vs=c(ax), exports=c(exp), level_verticalization = c(vert)) 
  format(df, digits=2, nsmall=2, scientific = FALSE)
  if(write.xlsx==TRUE)write.xlsx(df, file=name, sheetName="Sheet1", showNA=TRUE)
  list(format(df, digits=2, nsmall=2, scientific = FALSE), total.exp=(format(sum(exp), scientific = FALSE)), vs=(format(sum(ax)/xk, scientific = FALSE)))
}
