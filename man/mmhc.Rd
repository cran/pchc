\name{The MMHC and MMTABU Bayesian network learning algorithms}
\alias{mmhc}
\alias{mmtabu}
\title{
The MMHC and MMTABU Bayesian network learning algorithms
}

\description{
The MMHC and MMTABU Bayesian network learning algorithms.
}

\usage{
mmhc(x, method = "pearson", max_k = 3, alpha = 0.05, robust = FALSE,
skel = NULL, ini.stat = NULL, R = NULL, restart = 10, score = "bic-g",
blacklist = NULL, whitelist = NULL)

mmtabu(x, method = "pearson", max_k = 3, alpha = 0.05, robust = FALSE,
skel = NULL, ini.stat = NULL, R = NULL, tabu = 10, score = "bic-g",
blacklist = NULL, whitelist = NULL)
}

\arguments{
\item{x}{
A numerical matrix with the variables. If you have a data.frame (i.e. categorical data) turn them into a matrix
using \code{\link{data.frame.to_matrix}}. Note, that for the categorical case data, the numbers must start from 0.
No missing data are allowed.
}
\item{method}{
If you have continuous data, this "pearson". If you have categorical data though,
this must be "cat". In this case, make sure the minimum value of each variable is zero.
The function "g2Test" in the R package \bold{Rfast} and the relevant functions work that way.
}
\item{max_k}{
The maximum conditioning set to use in the conditional indepedence test (see Details). Integer, default value is 3
}
\item{alpha}{
The significance level for assessing the p-values.
}
\item{robust}{
Do you want outliers to be removed prior to applying the MMHC algorithm? If yes, set this to TRUE to utilise the MCD.
}
\item{skel}{
If you have the output of the skeleton phase, the output from the function \code{\link{mmhc.skel}} plug it here. This can save time.
}
\item{ini.stat}{
If the initial test statistics (univariate associations) are available, pass them through this parameter.
}
\item{R}{
If the correlation matrix is available, pass it here.
}
\item{restart}{
An integer, the number of random restarts.
}
\item{tabu}{
An integer, the length of the tabu list used in the tabu function.
}
\item{score}{
A character string, the label of the network score to be used in the algorithm. If none is specified,
the default score is the Bayesian Information Criterion for both discrete and continuous data sets.
The available score for continuous variables are: "bic-g" (default), "loglik-g", "aic-g", "bic-g" or "bge".
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
The MMHC algorithm is implemented without performing the backward elimination during the skeleton identification phase.
The MMHC as described in Tsamardinos et al. (2006) employs the MMPC algorithm during the skeleton construction phase and the Tabu search in the scoring phase. In this package, the mmhc function employs the Hill Climbing greedy search in the scoring phase while the mmtabu employs the Tabu search.
}

\value{
A list including:
\item{ini}{
A list including the output of the \code{ \link{mmhc.skel} } function.
}
\item{dag}{
A "bn" class output. A list including the outcome of the Hill-Climbing or the Tabu search phase. See the package "bnlearn" for more details.
}
\item{scoring}{
The score value.
}
\item{runtime}{
The duration of the algorithm.
}
}

\references{
Tsamardinos I., Brown E.L. and Aliferis F.C. (2006). The max-min hill-climbing Bayesian network
structure learning algorithm. Machine Learning, 65(1): 31-78.

Tsagris M. (2021). A new scalable Bayesian network learning algorithm with applications to economics.
Computational Economics, 57(1):341-367.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{fedhc}, \link{pchc}, \link{mmhc.skel}, \link{mmhc.boot} }
}

\examples{
# simulate a dataset with continuous data
x <- matrix( rnorm(300 * 30, 1, 10), nrow = 300 )
a <- mmhc(x)
}

