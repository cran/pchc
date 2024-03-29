\name{Lower limit of the confidence of an edge}
\alias{conf.edge.lower}
\title{
Lower limit of the confidence of an edge
}

\description{
Lower limit of the confidence of an edge.
}

\usage{
conf.edge.lower(p)
}

\arguments{
\item{p}{
A numerical vector with the proportion of times an edge was found in the bootstrapped PC algorithm or the confidence of the edge returned by \code{\link{bn.skel.utils2}}.
}
}

\details{
After having performed PC algorithm many times in the bootstrap samples (using \code{\link{mmhc.skel.boot}} for example) you get a symmetric matrix with the proportion of times an edge was discovered. Take the lower (or upper) triangular elements of that matrix and pass them as input in this function. This will tell you the minimum proportion required to be confident that an edge is trully significant.
}

\value{
The estimated cutoff limit above which an edge can be deemed significant.
}

\references{
Scutari M. and Nagarajan R. (2013). Identifying significant edges in graphical models of molecular networks. Artifficial Intelligence in Medicine, 57: 207-217.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{pchc.skel.boot}, \link{mmhc.skel.boot}, \link{fedhc.skel.boot}
}
}

\examples{
y <- pchc::rbn2(200, p = 30, nei = 3)
x <- y$x
g <- pchc::pchc.skel.boot(x, B = 100)$Gboot
a <- g[ lower.tri(g) ]
pchc::conf.edge.lower(a)
}

