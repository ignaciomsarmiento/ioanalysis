vs.ki<-function(imp,exp,out,namesector, write.xlsx=TRUE, name="Level_Verticalization.xlsx"){
  vert<-(imp/out)*exp
  df <- data.frame(sectors = c(namesector),  imports=c(imp), output=c(out), exports=c(exp), level_verticalization = c(vert)) 
  format(df, digits=2, nsmall=2, scientific = FALSE)
  if(write.xlsx==TRUE)write.xlsx(df, file=name, sheetName="Sheet1", showNA=TRUE)
  format(df, digits=2, nsmall=2, scientific = FALSE)
}
