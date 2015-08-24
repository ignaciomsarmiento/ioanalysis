\name{output.decomposition}
\alias{output.decomposition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Decomposition of Output Changes
}
\description{
The decomposition technique follows Sonis et a. (1996) and analyzes differences in two-period sectoral output.
}
\usage{
output.decomposition(mip0, mip1,X0, X1,f0,f1, write.xlsx=FALSE, name="Output_Decomposition.xlsx")
}

\arguments{
  \item{mip0}{Matrix. Input output matrix at time 0}
  \item{mip1}{Matrix. Input output matrix at time 1}
  \item{X0}{Vector. Input in each column at time 0}
  \item{X1}{Vector. Input in each column at time 1}
  \item{f0}{Vector. Final Demand at time 0}
  \item{f1}{Vector. Final Demand at time 1}
  \item{write.xlsx}{Logical. If TRUE writes an excel file}
  \item{name}{String. Name of the excel file}
}

\details{
 The output matrix will have nine columns identified as "Decom_1","Decom_2","Decom_3","Self_Ch_1",
 "Self_Ch_2","Self_Ch_3","Non_Self_Ch_1","Non_Self_Ch_2","Non_Self_Ch_3". 
 \itemize{
  \item{"Decom_1"}{ Refers to output changes that are due to changes in final demand}
  \item{"Decom_2"}{ Refers to output changes that are due to technological progress (i.e., due to changes in the Leontief inverse matrices_)}
  \item{"Decom_3"}{ Refers to output changes that are due to synergistic interaction between final demand and technological change}
  }
  
  Further, the decomposition can be made to trace the output changes by determining whether they originated from the sector itself or from other sectors in the economy. The two components are referred to as \emph{self-generated} and \emph{non-self-generated}
  \itemize{
  \item{"Self_Ch_1", "Self_Ch_2", "Self_Ch_3"}{ Refers to output changes that are self-generated}
  \item{"Non_Self_Ch_1", "Non_Self_Ch_2", "Non_Self_Ch_3"}{ Refers to output changes that are non-self-generated}
  }
}
\value{
Returns a matrix.
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-T-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-T-23.pdf})

Sonis, Michael & Geoffrey JD Hewings, & Jiemin Guo. \emph{Sources of structural change in input–output systems: a field of influence approach}. Economic Systems Research 8, no. 1 (1996): 15-32.
}

\author{
Ignacio Sarmiento-Barbieri
}

\seealso{
See Also  \code{\link{leontief.inv}}
}

\examples{
#Follows the example in PyIO 2.0 Quick Start


mip0<-matrix(c(1650.9,4.5,25553,0,660,1792.8,104.6,0,
              6.8,149.2,11820.9,485.7,2610.5,0.1,8.2,0,
              4317.5,827.6,33858.6,1301.7,17026.9,8186,5164.5,46.7,
              35.1,9.1,932.4,668.5,19.1,991.2,308.5,2,
              192.8,199.9,194.9,59.1,59.4,1602,160.7,0,
              1574.4,1573.7,9974.3,454.1,6690.5,11924.6,2096.3,6.6,
              259.2,222,476.3,49.5,44.9,2688.1,456.2,1,
              0,0,888,0,1.2,74.5,11.9,39.6), ncol=8, byrow=TRUE)

mip1<-matrix(c(16,5,24,0,6,17,10,0,7,17,11,48,26,0,
              8,0,43,82,33,13,17,81,51,4,35,9,93,7,
              19,99,30,2,19,20,19,6,59,16,16,0,15,15,
              99,45,66,11,12,7,25,22,47,4,42,26,45,1,
              0,0,75,0,12,7,12,3), ncol=8, byrow=TRUE)

X1<-c(700,320,607,432,375,345,561,187)
X0<-c(49770.9,28620.0,126463.8,4507.0,38907.7,89783.0,30094.2,173.1)
f1<-c(622,203,283,138,220,75,349,78)
f0<-c(20005.1,13538.6,55734.3,1541.1,36438.9,55488.5,25897,-842.1)


OD<-output.decomposition(mip0, mip1,X0, X1,f0,f1, write.xlsx=FALSE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


