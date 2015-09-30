\name{vs.ki}
\alias{vs.ki}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vertical Specialization
}
\description{
VS is the imported input content of exports for country k in sector i. The Vertical specialization chain is VS_ki=(imported intermediates/gross output)*exports. The first term is the share of imported inputs into gross production. Multiplying this ratio by the amount that is exported provides the monetary value for the imported input content of exports.

}
\usage{
vs.ki(imp,exp,out,namesector, write.xlsx=TRUE, name="Level_Verticalization.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imp}{Vector. Imported intermediates of sector i}
  \item{exp}{Vector. Exports}
  \item{out}{Vector. Gross Output}
  \item{namesector}{String. Name of Sector i}
  \item{write.xlsx}{Logical. if TRUE results are presented in an excel file}
  \item{name}{String. name of the excel file}
}

\details{
The dimension of the four arguments must coincide. 
}
\value{
Returns a data frame
}
\references{
Hummels, David & Ishii, Jun & Yi, Kei-Mu, 2001. \emph{The nature and growth of vertical specialization in world trade}. Journal of International Economics, Elsevier, vol. 54(1), pages 75-96, June.
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

y<-mat_40x40$DT.a.PB[1:40]
x<-mat_40x40$X[1:40]
m<-mat_40x40[42,2:41]


vs<-vs.ki(m,x,y, write.xlsx=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

