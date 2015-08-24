\name{gosh.inv}
\alias{gosh.inv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gosh Inverse
}
\description{
Computes the Goshian Inverse 
}
\usage{
gosh.inv(mip, X, write.xlsx=FALSE, name="Gosh_Inv.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mip}{Matrix. Input output matrix}
  \item{X}{Vector. Input in each column}
  \item{write.xlsx}{Logical. If TRUE writes an excel file}
  \item{name}{String. Name of the excel file}
}
\details{
The Goshian inverse is derived from the output allocation matrix table A=[a_ij] where
\deqn{a_ij=z_{ij}/X_i} 

where z_ij is the input from i required in the production of j. X_i is the corresponding output in each row. The Goshian inverse is then computed as 
\deqn{(I-A)^{-1}}

}
\value{
Returns a data.frame with the Goshian Inverse
}

\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})
}

\author{
Ignacio Sarmiento-Barbieri
}

\examples{

mip<-mat_40x40[1:40,2:41] #Input-output coeffcients

X<-mat_40x40$DT.a.PB[1:40]  #Total output vector

G<-gosh.inv(mip,X,write.xlsx = FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


