% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModularClusterExplore10x.R
\name{ClusterExplore10xServer}
\alias{ClusterExplore10xServer}
\title{ClusterExplore10xServer}
\usage{
ClusterExplore10xServer(input, output, session, outs)
}
\arguments{
\item{input}{List-like object that stores the current values of all of the widgets in your app.}

\item{output}{List-like object that stores instructions for building the R objects in your app.}

\item{session}{List-like object about the session to be passed to the UI.}

\item{outs}{a reactive object from the cellranger toolkit}
}
\description{
Runs and visulizes the 10X clustering from the Cell Ranger website.  This function is to render
the Server for the shiny module.
}
\details{
The module allows for the input of different parameters such as the number of clusters and limits
for the number of genes or the expression limits in the heatmap.
}
\seealso{
shiny
}
\author{
Kirk Gosik <kgosik@broadinstitute.org>
}
\keyword{shiny}
