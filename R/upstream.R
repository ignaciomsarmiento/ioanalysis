upstream<-function(A,X,E,IM, write.xlsx=TRUE){
	if(class(A)!="data.frame")stop("Should be element of a data.frame class")
	nosect<-dim(A)[1]
	x.j<-cbind(apply(X,2,sum))
	x.i<-cbind(apply(X,1,sum))
	int.sup<-(x.i-E+IM)
	for(i in 1:nosect) if(int.sup[i]==0) int.sup[i]=0.1
	w<-cbind((x.j/int.sup))
	dij<-A*w
	d.inv<-solve(diag(nosect)-dij)
	u<-cbind(rep(1,nosect))
	U<-d.inv%*%u
	if(write.xlsx==TRUE)write.xlsx(U, file="Upstream.xlsx", sheetName="Sheet1", showNA=TRUE)
	U
}