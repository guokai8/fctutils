% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_misc.R
\name{ft_level_lengths}
\alias{ft_level_lengths}
\title{Get Character Lengths of Factor Levels}
\usage{
ft_level_lengths(factor_vec)
}
\arguments{
\item{factor_vec}{A factor vector.}
}
\value{
A named numeric vector with the length of each level.
}
\description{
Calculates the number of characters in each level of a factor vector.
}
\examples{
# Example factor vector
factor_vec <- factor(c('apple', 'banana', 'cherry'))

# Get level lengths
ft_level_lengths(factor_vec)
}
\author{
Kai Guo
}
