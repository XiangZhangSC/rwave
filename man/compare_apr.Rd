% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_apr.R
\name{compare_apr}
\alias{compare_apr}
\title{ATP production rate comparison between groups}
\usage{
compare_apr(df, which_assay = "XFe96", stats.only = FALSE)
}
\arguments{
\item{df}{a \code{data.frame} with raw data and produced by \code{import_seahorse}}

\item{which_assay}{a \code{character} which can be "XFp", "XFe24", "XF" or "XFe96"}

\item{stats.only}{a \code{logical} value that indicates whether only the statistical outcomes are returned}
}
\description{
compare_apr compares APR between experimental groups
}
\details{
When there are two groups, the Wilcoxon signed-rank test will be used for the group comparison.
When there are three groups, the Kruskal–Wallis one-way analysis of variance will be sued for the group comparison.
}
