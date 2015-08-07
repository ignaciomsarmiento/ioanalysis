\name{extraction}
\alias{extraction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Implements Extraction Method
}
\description{
Applies the Extraction method as outlined in Dietzenbacher et al. (1993)
}
\usage{
extraction(mip, X, f, v, forward=FALSE, write.xlsx=FALSE, name="Extraction.xlsx")
}

\arguments{
  \item{mip}{Matrix. Input output matrix}
  \item{X}{Vector. Input in each column}
  \item{f}{Vector. Final demand vector}
  \item{v}{Vector. Primary input vector}
  \item{write.xlsx}{Logical. If TRUE writes an excel file}
  \item{name}{String. Name of the excel file}
}
\details{
The matrix should be read cautiously. Columns indicate the sector under analysis. So the first column gives teh impact when sector 1 is hypthetically extracted from the system. Thus A[1,1] is the impact to sector 1 when that sector is hypothetically extracted. A[1,2] gives the impact to sector 1 when sector 2 was hypothetically extracted.
A[2,1] represents the impact to sector 2 when sector 1 is hypothetically extracted and so on.

The diagonal of the output matrix represents Dietzenbacher's feedback effect.
}
\value{
Returns a matrix with the Extraction matrix
}
\references{
Dietzenbacher Erik & van der Linden Jan A. &  Steenge Alben E.  (1993). \emph{The Regional Extraction Method: EC Input–Output Comparisons}. Economic Systems Research. Vol. 5, Iss. 2, 1993

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#Follows the example in PyIO 2.0 Quick Start

mip<-matrix(c(16,5,24,0,6,17,10,0,7,17,11,48,26,0,8,0,43,82,33,13,17,81,51,4,35,9,93,7,19,99,30,2,19,20,19,6,59,16,16,0,15,15,99,45,66,11,12,7,25,22,47,4,42,26,45,1,0,0,75,0,12,7,12,3), ncol=8, byrow=TRUE)
mip

X<-c(700,320,607,432,375,345,561,187)


f<-c(622,203,283,138,220,75,349,78)

extraction(mip,X,f, v,forward=FALSE,write.xlsx=FALSE, name="Extraction.xlsx")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


