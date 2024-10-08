% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_misc.R
\name{ft_unique_comb}
\alias{ft_unique_comb}
\title{Create Factor of Unique Combinations from Multiple Factors}
\usage{
ft_unique_comb(..., sep = "_")
}
\arguments{
\item{...}{Factor vectors to combine.}

\item{sep}{A string to separate levels in the combined factor. Default is \code{'_'}.}
}
\value{
A factor vector representing unique combinations.
}
\description{
Generates a new factor where each level represents a unique combination of levels from the input factors.
}
\examples{
# Example factors
factor_vec1 <- factor(c('A', 'A', 'B', 'B'))
factor_vec2 <- factor(c('X', 'Y', 'X', 'Y'))

# Create unique combinations
combined_factor <- ft_unique_comb(factor_vec1, factor_vec2)
levels(combined_factor)
}
\author{
Kai Guo
}
