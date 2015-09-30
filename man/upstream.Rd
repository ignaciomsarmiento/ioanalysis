\name{upstream}
\alias{upstream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{upstreamness
}
\description{
Measures upstreamness as in Antràs et al. (2012), equation (9) page 5.
}
\usage{
upstream(D,y,x,m, write.xlsx=TRUE, name="Upstream.xlsx")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{D}{Matrix. Direct Domestic Coefficients Matrix}
  \item{y}{Vector. Output}
  \item{x}{Vector. Exports}
  \item{m}{Vector. Imports}
  \item{write.xlsx}{Logical. if TRUE results are presented in an excel file}
  \item{name}{String. name of the excel file}
}
\details{
Can use the IO matrix as D.
}
\value{
Returns a Vector with the Upstremness of each Industry
}
\references{
Pol Antràs & Davin Chor & Thibault Fally & Russell Hillberry, 2012. \emph{Measuring the Upstreamness of Production and Trade Flows}. NBER Working Papers 17819, National Bureau of Economic Research, Inc.
}

\author{
Ignacio Sarmiento-Barbieri
}

\examples{
#Uses the 40x40 matrix included in the package

mip<-mat_40x40[1:40,2:41] 
y<-mat_40x40$DT.a.PB[1:40]
x<-mat_40x40$X[1:40]
m<-mat_40x40[42,2:41]


upstream<-upstream(mip,y,x,m, write.xlsx=FALSE)

}

