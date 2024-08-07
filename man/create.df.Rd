% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions2.R
\name{create.df}
\alias{create.df}
\title{Generating correlations from all read data}
\usage{
create.df(
  data,
  cuts,
  freqs,
  freq_mult = 1.775,
  map_func = freq.map.radial,
  verb = TRUE
)
}
\arguments{
\item{data}{list of arrays where each elements is an array corresponding to all the data from a single knife, typically generated by myread.data}

\item{cuts}{numeric vector of cut points for specifying frequency ranges over which we would like to calculate correlations.
For example, if 5-10 and 10-20 frequency ranges are desired, set cuts as c(5,10,20).}

\item{freqs}{This is an alternative to specifying cuts (note that you should only specify one or the other). Suppose that you wished to calculate
correlations for the 5-10 and 20-30 frequency ranges, but not 10-20. Then you could use this argument by passing a matrix with two columns and a
 number of rows equal to the number of frequency ranges you want to perform calculations for. The first column indicates the lower number in the
 frequency range, and the second column specifies the upper number in the frequency range.}

\item{freq_mult}{This argument is the multiplier that tells you the at a given row or column index. This changes depending on the image
magnification.}

\item{map_func}{This argument is a function that takes in three arguments: freqs, freq\_mult, and image.dimensions, and produces a 3-D array
where, along the third dimension, each element is a matrix of zeros and ones specifying the cells in a given image that are relevant for
calculations at a given frequency range. Each of these matrices will be passed to func to specify the cells from a specific frequency range.
The default function used here will likely not need to change.}

\item{verb}{true or false, indicating if you want progress details}
}
\value{
data frame with the correlations along all frequencies, sample pairs
}
\description{
this function finds the correlations between base and tip along all the knifes, image repetitions, and desired frequencies.
}
\details{
This function creates a data frame which has columns containing some quantity (usually correlation) calculated for every pair
of aligned images on a base and a tip.
}
\author{
Carlos Llosa-Vite, \email{llosacarlos2@gmail.com}

Geoffrey Z. Thompson
}
