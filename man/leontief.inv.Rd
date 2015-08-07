\name{leontief.inv}
\alias{leontief.inv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Leontief Inverse
}
\description{
Computes the Leontief Inverse and  Backward and Forward Linkages
}
\usage{
leontief.inv(mip, X, linkages=TRUE, write.xlsx=TRUE, name="Leontief_Inv.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mip}{Matrix. Input output matrix}
  \item{X}{Vector. Input in each column}
  \item{linkages}{Logical. If TRUE computes Backward and Forward Linkages}
  \item{write.xlsx}{Logical. If TRUE writes an excel file}
  \item{name}{String. Name of the excel file}
}
\details{
The Leontief inverse is derived from the input-output table A=[a_ij] where
\deqn{a_ij=z_ij/X_j} 

where z_ij is the input from i required in the production of j. X_j is the corresponding input in each coulumn. The Leontief inverse is then computed as 
\deqn{(I-A)^{-1}}

The user has the option to compute the Backward and Forward Linkages for Key-sector Analysis. The sector j's Backward Linkage (\eqn{BL_{j}}) is defined as:

\deqn{BL_{j}=\frac{\frac{1}{n}\sum_{i=1}^{n}b_{ij}}{\frac{1}{n^{2}}\sum_{j=1}^{n}\sum_{i=1}^{n}b_{ij}}}
}

The sector j's Forward Linkage (\eqn{FL_{i}}) is defined as:


\deqn{FL_{i}=\frac{\frac{1}{n}\sum_{j=1}^{n}b_{ij}}{\frac{1}{n^{2}}\sum_{j=1}^{n}\sum_{i=1}^{n}b_{ij}}}


\value{
Returns a data.frame with the Leontief Inverse
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})

}
\author{
Ignacio Sarmiento-Barbieri
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

mip<-mat_40x40[1:40,2:41] #Input-output coeffcients

X<-mat_40x40$DT.a.PB[1:40]  #Total output vector

L<-leontief.inv(mip,X,linkages=TRUE,write.xlsx = FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


