\name{Markov blanket of a node in a Bayesian network}
\alias{mb}
\title{
Markov blanket of a node in a Bayesian network
}

\description{
Markov blanket of a node in a Bayesian network.
}

\usage{
mb(bn, node)
}

\arguments{
\item{bn}{
This can either be a bn object or the adjacency matrix.
}
\item{node}{
A vector with one number indicating the node or variable whose Markov blanket is to be returned.
}
}

\details{
The Markov blanket of a variable (node) is the set of its parents, children and spouses.
}

\value{
\item{parents}{
The parents of the node of interest.
}
\item{children}{
The children of the node of interest.
}
\item{spouses}{
The spouses of the node of interest. These are the other parents of the children of the node of interest.
}
\item{markov.blanket}{
The Markov blanket of the node of interest. The collection of all the previous.
}
}

%\references{
%
%}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{pchc}, \link{fedhc}, \link{mmhc}, \link{bnplot}
}
}

\examples{
\donttest{
y <- pchc::rbn3(1000, 10, 0.3)
tru <- y$G
x <- y$x
mod <- pchc(x)
pchc::bnplot(mod$dag)
G <- pchc::bnmat(mod$dag)
pchc::mb(G, 6)
}
}

