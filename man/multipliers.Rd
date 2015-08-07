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
\deqn{w_{n+1,i}=\frac{e_{i}}{X_{i}}}{ w_(n+1,i)=e_i / X_i}

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
Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})

Wu, P.C (2009). "PyIO 2.0 Quick Start". (\url{http://www.real.illinois.edu/pyio/})
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
#Follows the example in PyIO 2.0 Quick Start


mip<-matrix(c(16,5,24,0,6,17,10,0,7,17,11,48,26,0,
              8,0,43,82,33,13,17,81,51,4,35,9,93,7,
              19,99,30,2,19,20,19,6,59,16,16,0,15,15,
              99,45,66,11,12,7,25,22,47,4,42,26,45,1,
              0,0,75,0,12,7,12,3), ncol=8, byrow=TRUE)

X<-c(700,320,607,432,375,345,561,187)

e<-c(10,20,30,52,10,75,51,40)

z<-c(29870.9,18720,66563.8,2607,19007.7,69883,10194.2,173.1)

L<-multipliers(mip=mip,X=X,z=z,e=e)

}


