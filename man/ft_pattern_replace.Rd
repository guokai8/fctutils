% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_misc.R
\name{ft_pattern_replace}
\alias{ft_pattern_replace}
\title{Replace Patterns in Factor Levels (Deprecated)}
\usage{
ft_pattern_replace(factor_vec, pattern, replacement)
}
\arguments{
\item{factor_vec}{A factor vector to modify.}

\item{pattern}{A regular expression pattern to match.}

\item{replacement}{A string to replace the matched patterns.}
}
\value{
A factor vector with modified levels.
}
\description{
This function is deprecated. Please use \code{\link{ft_replace_pattern}} instead.
}
\examples{
# Deprecated: Use ft_replace_pattern instead
factor_vec <- factor(c('user_123', 'admin_456', 'guest_789'))
ft_replace_pattern(factor_vec, pattern = '[0-9]+', replacement = 'ID')
}
\author{
Kai Guo
}
\keyword{internal}
