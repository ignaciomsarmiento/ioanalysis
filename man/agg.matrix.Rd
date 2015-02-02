\name{agg.matrix}
\alias{agg.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Aggregation Matrix
}
\description{
Aggregates Inpuy-Output Matrices. Construct an agregation matrix of kxn dimension. 
}
\usage{
agg.matrix<-function(mat,d.mip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{Matrix. How sectors should be agregated. First column should be the new sector in order, the remaining columns the sectors that are foing to be agregated}
  \item{dimcol}{Numeric. Column dimension}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
temp<-matrix(sample(1:40), 14)
temp[,1]<-seq(1:14)
S<-agg.matrix(temp,40)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aggregate }

