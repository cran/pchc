\name{Bootstrap versions of the skeleton of a Bayesian network}
\alias{pchc.skel.boot}
\alias{fedhc.skel.boot}
\alias{mmhc.skel.boot}
\title{
Bootstrap versions of the skeleton of a Bayesian network
}

\description{
Bootstrap versions of the skeleton of a Bayesian network.
}

\usage{
pchc.skel.boot(x, method = "pearson", alpha = 0.05, B = 200)
fedhc.skel.boot(x, method = "pearson", alpha = 0.05, B = 200)
mmhc.skel.boot(x, max_k = 3, method = "pearson", alpha = 0.05, B = 200)
}

\arguments{
\item{x}{
A matrix with the variables. The user must know if they are continuous or if they are categorical. \bold{If you have categorical data though, the user must transform the data.frame into a matrix. In addition, the numerical matrix must have values starting from 0. For example, 0, 1, 2, instead of "A", "B" and "C"}.
}
\item{max_k}{
The maximum conditioning set to use in the conditional indepedence test (see Details). Integer, default value is 3.
}
\item{method}{
If you have continuous data, this "pearson". If you have categorical data though,
this must be "cat". In this case, make sure the minimum value of each variable is zero. The function "g2Test" in the R package \bold{Rfast} and the relevant functions work that way.
}
\item{alpha}{
The significance level ( suitable values in (0, 1) ) for assessing the p-values. The default value is 0.05.
}
\item{B}{
The number of bootstrap resamples to draw. The algorithm is performed in each bootstrap sample. In the end, the adjacency matrix on the observed data is returned, along with another adjacency matrix produced by the bootstrap. The latter one contains values from 0 to 1 indicating the proportion of times an edge between two nodes was present.
}
}

%details{
%
%}

\value{
A list including:
\item{G}{
The observed adjancency matrix. A value of 1 in G[i, j] appears in G[j, i] also, indicating that i and j have an edge between them.
}
\item{Gboot}{
The bootstrapped adjancency matrix. A value of 1 in G[i, j] appears in G[j, i] also, indicating that i and j have an edge between them.
}
\item{runtime}{
The duration of the algorithm.
}
}

\references{
A new scalable Bayesian network learning algorithm with applications to economics. 
Computational Economics, 57(1): 341-367.

Tsagris M. (2022). The FEDHC Bayesian Network Learning Algorithm. Mathematics 2022, 10(15), 2604.

Spirtes P., Glymour C. and Scheines R. (2001). Causation, Prediction, and Search. 
The MIT Press, Cambridge, MA, USA, 3nd edition.

Tsamardinos I., Brown E.L. and Aliferis F.C. (2006). The max-min hill-climbing Bayesian network 
structure learning algorithm. Machine learning 65(1): 31-78.

Borboudakis G. and Tsamardinos I. (2019). Forward-backward selection with early dropping. 
Journal of Machine Learning Research, 20(8): 1-39.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{pchc.skel}, \link{fedhc.skel}, \link{mmhc.skel}, \link{bn.skel.utils}
}
}

\examples{
\donttest{
x <- pchc::rbn2(500, p = 20, nei = 3)$x
a <- pchc::pchc.skel.boot(x, alpha = 0.05)
}
}

\keyword{ pc algorithm }
