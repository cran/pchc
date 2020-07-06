\name{The PCHC Bayesian network learning algorithm}
\alias{pchc}
\title{
The PCHC Bayesian network learning algorithm
}

\description{
The PCHC Bayesian network learning algorithm.
}

\usage{
pchc(x, method = "pearson", alpha = 0.05, ini.stat = NULL,
ini.pvalue = NULL, restart = 10, score = "bge", blacklist = NULL, whitelist = NULL)
}

\arguments{
\item{x}{
A numerical matrix with the variables. If you have a data.frame (i.e. categorical data) turn them into a matrix
using \code{\link{data.frame.to_matrix}}. Note, that for the categorical case data, the numbers must start from 0. No missing data are allowed.
}
\item{method}{
If you have continuous data, you can choose either "pearson" or "spearman". If you have categorical data though, this must be "cat". In this case, make sure the minimum value of each variable is zero. The \code{\link{g2Test}} and the relevant functions work that way.
}
\item{alpha}{
The significance level for assessing the p-values.
}
\item{ini.stat}{
If the initial test statistics (univariate associations) are available, pass them through this parameter.
}
\item{ini.pvalue}{
if the initial p-values of the univariate associations are available, pass them through this parameter.
}
\item{restart}{
An integer, the number of random restarts.
}
\item{score}{
A character string, the label of the network score to be used in the algorithm. If none is specified,
the default score is the Bayesian Information Criterion for both discrete and continuous data sets.
The available score for continuous variables are: "bge" (default), "loglik-g", "aic-g", "bic-g" or "bge".
The available score categorical variables are: "bde", "loglik" or "bic".
}
\item{blacklist}{
A data frame with two columns (optionally labeled "from" and "to"), containing a set of arcs not to
be included in the graph.
}
\item{whitelist}{
A data frame with two columns (optionally labeled "from" and "to"), containing a set of arcs to be
included in the graph.
}
}

\details{
The PC algorithm as proposed by Spirtes et al. (2000) is first implemented followed by a scoring phase, such as hill climbing.
}

\value{
A list including:
\item{stat}{
The test statistics of the univariate associations.
}
\item{ini.pvalue}{
The initial p-values univariate associations.
}
\item{pvalue}{
The logarithm of the p-values of the univariate associations.
}
\item{runtime}{
The amount of time it took to run the algorithm.
}
\item{kappa}{
The maximum value of k, the maximum cardinality of the conditioning set at which the algorithm stopped.
}
\item{n.tests}{
The number of tests conducted during each k.
}
\item{G}{
The adjancency matrix. A value of 1 in G[i, j] appears in G[j, i] also, indicating that i and j have an edge between them.
}
\item{sepset}{
A list with the separating sets for every value of k.
}
}
\references{
Spirtes P.,  Glymour C. and Scheines R. (2001). Causation, Prediction, and Search. The MIT Press, Cambridge, MA, USA, 3nd edition.

Tsamardinos I., Borboudakis G. (2010) Permutation Testing Improves Bayesian Network Learning. In Machine Learning
and Knowledge Discovery in Databases. ECML PKDD 2010. 322-337.

Tsamardinos I., Brown E.L. and Aliferis F.C. (2006). The max-min hill-climbing Bayesian network
structure learning algorithm. Machine learning 65(1):31-78.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{pchc.skel} }
}

\examples{
# simulate a dataset with continuous data
x <- matrix( rnorm(200 * 50, 1, 10), nrow = 200 )
a <- pchc(x)
}

