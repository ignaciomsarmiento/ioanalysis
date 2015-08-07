\name{influence}
\alias{influnce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Field of Influence
}
\description{
Computes the first order field of Influence
}
\usage{
influence(mip,X, col,row, write.xlsx=FALSE, name="Field_of_Influence.xlsx")
}

\arguments{
  \item{mip}{Matrix. Input output matrix}
  \item{X}{Vector. Input in each column}
  \item{col}{Numeric. Column where the change is occurring}
  \item{row}{Numeric. Row where the change is occurring}
  \item{write.xlsx}{Logical. If TRUE writes an excel file}
  \item{name}{String. Name of the excel file}
}

\details{
The Field of influence assesses the changes in the Leontief inverse matrix resulting from the change in one or more direct input coefficients in the inverse Leontief matrix. 

The first order field of influence deals with a single change in one elements of the coefficient matrix.
}
\value{
Returns a matrix.
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})

}

\author{
Ignacio Sarmiento-Barbieri
}

\seealso{
See Also  \code{\link{leontief.inv}}
}

\examples{
#Follows the example in PyIO 2.0 Quick Start

mip<-matrix(c(16,5,24,0,6,17,10,0,7,17,11,48,26,0,
              8,0,43,82,33,13,17,81,51,4,35,9,93,7,
              19,99,30,2,19,20,19,6,59,16,16,0,15,15,
              99,45,66,11,12,7,25,22,47,4,42,26,45,1,
              0,0,75,0,12,7,12,3), ncol=8, byrow=TRUE)

X<-c(700,320,607,432,375,345,561,187)

F<-influence(mip,X,1,1,write.xlsx = FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


