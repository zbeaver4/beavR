% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/igraph_functions.R
\name{retrieve_edge_attributes}
\alias{retrieve_edge_attributes}
\title{Retrieve all edge attributes}
\usage{
retrieve_edge_attributes(g, index = NULL)
}
\arguments{
\item{g:}{graph to use}

\item{index:}{numeric or named edge indicating the relationship(s) of interest}
}
\description{
Given edge indices, retrieve all edge attributes for those nodes
}
\examples{
retrieve_edge_attributes(my_graph, 1:10)
}
\keyword{graph,}
\keyword{igraph}

