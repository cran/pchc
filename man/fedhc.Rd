\name{The FEDHC Bayesian network learning algorithm}
\alias{fedhc}
\title{
The FEDHC Bayesian network learning algorithm
}

\description{
The FEDHC Bayesian network learning algorithm.
}

\usage{
fedhc(x, method = "pearson", alpha = 0.05, robust = FALSE, ini.stat = NULL,
R = NULL, restart = 10, score = "bic-g", blacklist = NULL, whitelist = NULL)
}

\arguments{
\item{x}{
A numerical matrix with the variables. If you have a data.frame (i.e. categorical data) turn them into a matrix
using \code{\link{data.frame.to_matrix}}. Note, that for the categorical case data, the numbers must start from 0. No missing data are allowed.
}
\item{method}{
If you have continuous data, you can choose either "pearson" or "spearman". If you have categorical data though, this must be "cat". In this case, make sure the minimum value of each variable is zero. The \code{\link{g2test}} and the relevant functions work that way.
}
\item{alpha}{
The significance level for assessing the p-values.
}
\item{robust}{
Do you want outliers to be removed prior to applying the FEDHC algorithm? If yes, set this to TRUE to utilise the MCD.
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
The FEDHC algorithm.
}

\value{
A list including:
\item{ini}{
A list including the output of the \code{ \link{mmhc.skel} } function.
}
\item{dag}{
A "bn" class output. A list including the outcome of the Hill-Climbing phase. See the package "bnlearn" for more details.
}
\item{scoring}{
The score value.
}
\item{runtime}{
The duration of the algorithm.
}
}

\references{
Tsagris M. (2020). The FEDHC Bayesian network learning algorithm. https://arxiv.org/pdf/2012.00113.pdf.

Borboudakis G. and Tsamardinos I. (2019). Forward-backward selection with early dropping. Journal of Machine Learning Research, 20(8): 1-39.

Tsamardinos I., Brown E.L. and Aliferis F.C. (2006). The max-min hill-climbing Bayesian network
structure learning algorithm. Machine Learning 65(1):31-78.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{pchc}, \link{mmhc}, link{fedhc.skel} }
}

\examples{
# simulate a dataset with continuous data
x <- matrix( rnorm(200 * 50, 1, 10), nrow = 200 )
a <- fedhc(x)
}
