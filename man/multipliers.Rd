\name{multipliers}
\alias{multipliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiplier Analysis
}
\description{

According to Nazara et al. (2003) and  Blair and Miller (2009) four multipliers can be calculated from an input-output matrix: output multiplier, input multiplier, income multiplier and employment multiplier.

Output multiplier: it is computed from the Leontief inverse.   Let \deqn{B=[b_{ij}]} be the Leontief inverse matrix  the output multiplier for sector j, 
\deqn{O_{j}=\sum_{i=1}^{n}b_{ij}}

Input multiplier: it is computed from the Goshian inverse.  Let \deqn{G=[g_{ij}]} be the Goshian inverse matrix  the input multiplier for sector j, 
\deqn{I_{j}=\sum_{i=1}^{n}g_{ij}}

Income multiplier: the calculation of this multiplier requires a wage vector (z) to calculate the household input coefficient (a):
\deqn{a_{n+1,i}=\frac{z_{n+1,i}}{X_{i}}}

with the Leontief inverse, the household income multiplier for sector j is

\deqn{H_{j}=\sum_{i=1}^{n}a_{n+1,i}b_{ij}}

Employment multiplier: the calculation of this multiplier requires a sectoral employment vector (e) to calculate the labor input coefficient (w):
\deqn{w_{n+1,i}=\frac{e_{i}}{X_{i}}}

with the Leontief inverse, the employment  multiplier for sector j is

\deqn{E_{j}=\sum_{i=1}^{n}w_{n+1,i}b_{ij}}

}

\usage{
multipliers(mip, X, z, e,write.xlsx=TRUE, name="output_multiplier.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mip}{Input-output matrix}
  \item{X}{Vector. Total input or output}
  \item{z}{Vector. Household input coefficient}
  \item{e}{Vector. If TRUE identifies key sectors}
  \item{write.xlsx}{Logical. If TRUE writes an excel file}
  \item{name}{String. Name of the excel file}
%%     ~~Describe \code{x} here~~
}


\value{
  Returns a data frame with the calculated multipliers for each sector
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})

Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press
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
X<-as.matrix(mat_40x40$DT.a.PB[1:40])  #Total output vector

z<-mat_40x40$X[1:40] #

e<-mat_40x40$Chog[1:40]

multipliers<-multipliers(mip,X,z=z,e=e, write.xlsx=FALSE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multipliers }

