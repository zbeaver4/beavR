% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/outlier_detection_functions.R
\name{cade_outlier}
\alias{cade_outlier}
\title{Get outliers using Classifier-Adjusted Density Estimation (CADE)}
\usage{
cade_outlier(df, prop = 1, skip = c(), range = seq(0.3, 0.7, 0.1), ...)
}
\arguments{
\item{df:}{input data.frame}

\item{prop:}{proportion of fake to real data}

\item{skip:}{which columns the algorithm should skip}

\item{range:}{range of cutoffs to try with cade and push to the user}
}
\description{
Given a data.frame with a set of observations, determines which observations are likely to be outliers using CADE.
}

