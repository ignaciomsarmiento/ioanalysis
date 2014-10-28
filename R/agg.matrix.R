agg.matrix<-function(mat,d.mip){
  mat[is.na(mat)] <- 0
  s<-array(0,c(dim(mat)[1],d.imp))
  for (i in 1:nrow(mat)){
    for (j in 2:ncol(mat)){
      if(max(mat[i,j])!=0){s[i,max(mat[i,j])]<-1}
    }
  }
  s
}