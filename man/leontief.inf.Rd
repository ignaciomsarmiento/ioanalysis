\name{leontief.inv}
\alias{leontief.inv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Leontief Inverse
}
\description{
Computes the Leontief Inverse and the Backward Linkage
}
\usage{
leontief.inv(mip,X.j,write.xlsx=TRUE,name="Leontief_Inv.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mip}{matrix. input output matrix}
  \item{X.j}{vector. input in each column}
  \item{write.xlsx}{logical. if TRUE writes an excel file}
  \item{name}{string. name of the excel file}
}
\details{
The Leontief inverse is derived from the input-output table $A=[a_{ij}$ where
$$ a_{ij}=\frac{z_{ij}}{X_{j}} $$

where $z_{ij} is the input from $i$ required in the production of $j$. $X_{j}$ is the corresponding input in each coulumn. The leontief inverse is then computed as $(I-A)^{-1}$

Falta describir Backward Linkage
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Nazara, Guo, Hewing and Dridi (2003). \emph{PyIO. Input-Output Analysis with Python}. \url{http://www.real.illinois.edu/pyio/} 
}
\author{
Ignacio Sarmiento-Baribier
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~leontief inverse }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
