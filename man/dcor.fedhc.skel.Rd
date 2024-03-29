\name{Skeleton of the FEDHC algorithm using the distance correlation}
\alias{dcor.fedhc.skel}
\title{
The skeleton of a Bayesian network produced by the FEDHC algorithm using the distance correlation
}

\description{
The skeleton of a Bayesian network produced by the FEDHC algorithm using the distance correlation.
}

\usage{
dcor.fedhc.skel(x, alpha = 0.05, ini.stat = NULL, R = NULL)
}

\arguments{
\item{x}{
A numerical matrix with the variables. If you have a data.frame (i.e. categorical data) turn them
into a matrix using \code{\link{data.frame.to_matrix}}. Note, that for the categorical case data,
the numbers must start from 0. No missing data are allowed.
}
\item{alpha}{
The significance level (suitable values in (0, 1)) for assessing the p-values. Default value is 0.05.
}
\item{ini.stat}{
If the initial test statistics (univariate associations) are available, pass them through this parameter.
}
\item{R}{
If the correlation matrix is available, pass it here.
}
}

\details{
As in FEDHC the first phase consists of a variable selection procedure,
the FBED algortihm (Borboudakis and Tsamardinos, 2019) which is performed
though by utilizing the distance correlation (Szekely et al., 2007,
Szekely and Rizzo 2014, Huo and Szekely, 2016).
}

\value{
A list including:
\item{ini.stat}{
The test statistics of the univariate associations.
}
\item{ini.pvalue}{
The initial p-values univariate associations.
}
\item{pvalue}{
A matrix with the logarithm of the p-values of the updated associations.
This final p-value is the maximum p-value among the two p-values in the end.
}
\item{runtime}{
The duration of the algorithm.
}
\item{ntests}{
The number of tests conducted during each k.
}
\item{G}{
The adjancency matrix. A value of 1 in G[i, j] appears in G[j, i] also, indicating that i and j
have an edge between them.
}
}
\references{
Tsagris M. (2022). The FEDHC Bayesian Network Learning Algorithm.
Mathematics, 10(25): 2604.

Szekely G.J., Rizzo M.L. and Bakirov N.K. (2007). Measuring and Testing Independence
by Correlation of Distances. Annals of Statistics, 35(6):2769-2794.

Szekely G.J. and Rizzo M. L. (2014). Partial distance correlation with methods for dissimilarities.
Annals of Statistics, 42(6), 2382-2412.

Huo X. and Szekely G.J. (2016). Fast computing for distance covariance.
Technometrics, 58(4), 435-447.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{fedhc.skel}, \link{fedhc.skel.boot} }
}

\examples{
# simulate a dataset with continuous data
x <- matrix( rnorm(500 * 30, 1, 10), nrow = 500 )
a <- dcor.fedhc.skel(x)
}

