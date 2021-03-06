% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/topo.R
\name{topo}
\alias{topo}
\title{Create a random topography}
\usage{
topo(i, j)
}
\arguments{
\item{x}{length of surface}

\item{y}{width of surface}
}
\value{
a random surfacce \code{height} ("Value" section)
}
\description{
Create a surface with 5 sand piles randomly distributed on it
}
\details{
\code{topo} generates a vector with 0 indicating bare surface, and
             2 indicating the height of sand piles.
             Sand piles have a radius of 5m, and a hight of 2m.
}
\examples{
height=topo(100,100)
}

