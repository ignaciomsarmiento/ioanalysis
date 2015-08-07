\name{agg.matrix}
\alias{agg.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Aggregation Matrix
}
\description{
Aggregates Inpuy-Output Matrices. Construct an agregation matrix of kxn dimension. 
}
\usage{
agg.matrix(mat,d.mip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{Matrix. How sectors should be agregated. First column should be the new sector in order, the remaining columns the sectors that are going to be agregated}
  \item{dimcol}{Numeric. Column dimension}
}

\value{
Returns matrix to be pre and post multiplied to aggregate sectors.
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})

}
\author{
Ignacio Sarmiento-Barbieri
}

\examples{
temp<-matrix(sample(1:30), 10)
temp[,1]<-seq(1:10)

S<-agg.matrix(temp,40)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


