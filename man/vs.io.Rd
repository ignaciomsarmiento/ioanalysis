\name{vs.io}
\alias{vs.io}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VS share of total exports
}
\description{

Vertical Specialization (VS) share of total exports. The formula for VS as a share fo total exports for country k is
\deqn{VS share of total exports= VS/Xk=A*L*X/Xk}
where A is the nxn imported coefficient matrix,L is the Leontief inverse, X is an nx1 vector of exports, n is the number of secotrs, adn Xk is the sum of exports across the n sectors. 
}
\usage{
vs.io(imp,exp,leon=1,namesector, write.xlsx=TRUE, name="Level_Verticalization.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imp}{Is the nxn imported coefficient matrix}
  \item{exp}{Numeric Vector. Exports}
  \item{leon}{Leontief Inverse as output of funciton leontief.inv()}
  \item{namesector}{Character. Name of Sector i}
  \item{write.xlsx}{Logical. if TRUE writes an excel file}
  \item{name}{String. name of the excel file}
%%     ~~Describe \code{x} here~~
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a data frame with the A*L*X product, the total exports and the level of verticalization for each sector. Total.exp. returns a scalar, equals to the sum of exports across sectors
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
See Also  \code{\link{leontief.inv}}
}
\examples{
#Uses the 40x40 matrix included in the package

exp<-mat_40x40$X[1:40] #Exports Vector
class(exp) #the class is numeric
imp<-mat_imp_40x40[1:40,2:41]/mat_40x40$DT.a.PB[1:40] #Imports Coef Matrix
class(imp) #the class is data.frame
namesector<-colnames(mat_40x40[2:41]) 
class(namesector) #character

hum<-vs.io(imp,exp,namesector, write.xlsx=FALSE)

#Using the Leontief Inverse
leon<-leontief.inv(mat_40x40[1:40,2:41], as.matrix(mat_40x40$DT.a.PB[1:40]))[,1:40]
hum2<-vs.io(imp,exp,namesector,leon=leon, write.xlsx=FALSE)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ vs }

