% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_misc.R
\name{ft_rollup}
\alias{ft_rollup}
\title{Aggregate Factor Levels Based on Grouping}
\usage{
ft_rollup(factor_vec, groups)
}
\arguments{
\item{factor_vec}{A factor vector to aggregate.}

\item{groups}{A vector of the same length as \code{factor_vec} indicating group assignments.}
}
\value{
A factor vector with aggregated levels.
}
\description{
Aggregates the levels of a factor vector based on another grouping vector.
}
\examples{
# Example factor vector and groups
factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'fig'))
groups <- c('fruit', 'fruit', 'fruit', 'dry fruit', 'dry fruit')

# Aggregate levels based on groups
ft_rollup(factor_vec, groups)
}
\author{
Kai Guo
}
