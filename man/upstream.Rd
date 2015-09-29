\name{upstream}
\alias{upstream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{upstreamness
}
\description{
Measures upstreamness as in Antràs et al. (2012), equation (9) page 5.
}
\usage{
upstream(mip,y,x,m, write.xlsx=TRUE, name="Upstream.xlsx")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{mip}{Matrix. Input output matrix}
  \item{y}{Vector. Output}
  \item{x}{Vector. Exports}
  \item{m}{Vector. Imports}
  \item{write.xlsx}{Logical. if TRUE results are presented in an excel file}
  \item{name}{String. name of the excel file}
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
Pol Antràs & Davin Chor & Thibault Fally & Russell Hillberry, 2012. \emph{Measuring the Upstreamness of Production and Trade Flows}. NBER Working Papers 17819, National Bureau of Economic Research, Inc.
}

\author{
Ignacio Sarmiento-Barbieri
}

\examples{
##---TBC
}

