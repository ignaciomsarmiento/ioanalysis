agg.matrix<-function(mat,dimcol){
  mat[is.na(mat)] <- 0
  s<-array(0,c(dim(mat)[1],dimcol))
  for (i in 1:nrow(mat)){
    for (j in 2:ncol(mat)){
      if(max(mat[i,j])!=0){s[i,max(mat[i,j])]<-1}
    }
  }
  s
}



