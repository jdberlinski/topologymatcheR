% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions2.R
\name{myread.data}
\alias{myread.data}
\title{Read FFT data in Excel form}
\usage{
myread.data(
  img.no,
  sample.names,
  base.names,
  tip.names = base.names,
  base.directory,
  tip.directory = base.directory,
  surface = c("Base", "Tip"),
  verb = T,
  stopmiss = T
)
}
\arguments{
\item{img.no}{number of (usually) overlapping images}

\item{sample.names}{This is another character vector where each element is a string corresponding to the knives we wish to include.
In the image.names example, the element of this vector corresponding to knife T01 would be T01.}

\item{base.names}{names of the base images. Usually if operator A did it is A_FFT}

\item{tip.names}{usually the same name as the base, but might change.}

\item{base.directory}{The file path for where the bases are located}

\item{tip.directory}{typically the same as base.directory}

\item{surface}{Typically  c("Base", "Tip"), but this might change. For instance they might be  c("base", "tip") instead}

\item{verb}{show progress? True or False}

\item{stopmiss}{should the function stop when there is missing data?}
}
\value{
list of arrays, one for each knife, with all the read data across base x tip and image repetitions
}
\description{
The raw data for each image exists as a 512 by 1023 matrix (say) stored in an excel file (one file per image). This function
loops through the files in the specified folder/directory and stores the image matrices for each knife as a 4-D array which is one element
of a list. The array dimensions correspond to 1) Base/Tip 2) Image matrix rows 3) #' Image matrix columns 4) Image number.
}
\author{
Carlos Llosa

Geoffrey Z. Thompson
}
