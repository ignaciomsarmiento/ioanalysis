\name{key.sector}
\alias{key.sector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Impact Analysis: Backward and Forward linkages
}
\description{

Computation of Backward and Forward linkages. It aims to identify thos sectors whose economic activity exerts a greater than average influence on the whole economy. Key sectors are identified by calculating backard and forward linkages. 
Let \deqn{B=(I-A)^{-1}=[b_{ij}]} be the Leontief inverse matrix and let \eqn{B_{j}} and \eqn{B_{i}} be the column and row multipliers of this Leontief inverse. The sector j's backward linkage (\eqn{BL_{j}}) and forward linkage (\eqn{FL_{i}}) are defined as:

\deqn{BL_{j}=\frac{\frac{1}{n}\sum_{i=1}^{n}b_{ij}}{\frac{1}{n^{2}}\sum_{j=1}^{n}\sum_{i=1}^{n}b_{ij}}}
 

\deqn{FL_{i}=\frac{\frac{1}{n}\sum_{j=1}^{n}b_{ij}}{\frac{1}{n^{2}}\sum_{j=1}^{n}\sum_{i=1}^{n}b_{ij}}}
 

The usual interpretation is to propose that, if \deqn{BL_{j}>1}, a unit change in final demand in sector j will generate an above-average increase in activity in the economy. Similarly, for \deqn{FL_{i}>1}, it is asserted that a unit change in all sector's final demand would create an above average increase in sector i. Thus, a key sector is identified as one having both indices grater than one.
}

\usage{
key.sector(mip, X, epsilon=0.1, key=TRUE, cutoff=1, write.xlsx=TRUE, name="Key_sector.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mip}{Input-output matrix}
  \item{X}{Total input or output}
  \item{epsilon}{Replaces zeros in X}
  \item{key}{Logical. If TRUE identifies key sectors}
  \item{cutoff}{Cutoff above which are the key sectors}
  \item{write.xlsx}{Logical. If TRUE writes an excel file}
  \item{name}{String. Name of the excel file}
%%     ~~Describe \code{x} here~~
}

\details{
The function takes the sector names from the column names of the Input-output matrix
}
\value{
  Returns a vector with the calculated Backward and Forward linkages for each sector
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})
}
\author{
Ignacio Sarmiento-Barbieri
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also  \code{\link{leontief.inv}} \code{\link{gosh.inv}}
}
\examples{
#Uses the 40x40 matrix included in the package

mip<-mat_40x40[1:40,2:41] #Input-output coeffcients
X<-mat_40x40$DT.a.PB[1:40]  #Total output vector

key<-key.sector(mip,X, key=FALSE, write.xlsx=FALSE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ key sector }

