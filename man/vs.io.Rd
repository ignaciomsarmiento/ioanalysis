\name{vs.io}
\alias{vs.io}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VS share of total exports
}
\description{
VS share of total exports. The formula for VS as a share fo total exports for country k is
VS share of total exports= VS/Xk=A*X/Xk
where A is the nxn imported coefficient matrix, X is an nx1 vector of exports, n is the number of secotrs, adn Xk is the sum of exports across the n sectors. Elements a_ij of A denotes the imported inputs from sector i used to produce one unit of sectors j output.
}
\usage{
vs.io(imp,exp,namesector, write.xlsx=TRUE, name="Level_Verticalization.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imp}{Matrix. A is the nxn imported coefficient matrix}
  \item{exp}{Vector. Exports}
  \item{namesector}{String. Name of Sector i}
  \item{write.xlsx}{Logical. if TRUE writes an excel file}
  \item{name}{String. name of the excel file}
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{returns a data frame with the A*X product, the total exports and the level of verticalization for each sector}
  \item{total.exp. returns a scalar, equals to the sum of exports across secotrs}
}
\references{
Hummels, David & Ishii, Jun & Yi, Kei-Mu, 2001. "The nature and growth of vertical specialization in world trade," Journal of International Economics, Elsevier, vol. 54(1), pages 75-96, June.
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
