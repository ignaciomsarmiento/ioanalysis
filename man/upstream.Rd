\name{upstream}
\alias{upstream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{upstreamness
}
\description{
Measures upstreamness as of equation 9 in Antras et al. (2012)
}
\usage{
upstream(linv,y,x,m, write.xlsx=TRUE, name="Upstream.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{linv}{Matrix. Input output matrix}
  \item{y}{Output Vector}
  \item{x}{Exports Vector}
  \item{m}{Imports Vector}
  \item{write.xlsx}{Logical. if TRUE writes an excel file}
  \item{name}{String. name of the excel file}
}
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
Pol Antràs & Davin Chor & Thibault Fally & Russell Hillberry, 2012. "Measuring the Upstreamness of Production and Trade Flows," NBER Working Papers 17819, National Bureau of Economic Research, Inc.
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
