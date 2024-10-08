% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MLfuncs.R
\name{MLmatrixt_ar1}
\alias{MLmatrixt_ar1}
\title{Maximum Likelihood Estimation of the Matrix-t Distribution}
\usage{
MLmatrixt_ar1(
  Yall,
  maxit = 10000,
  df = 5,
  err = 1e-05,
  init = NULL,
  samecol = TRUE
)
}
\arguments{
\item{Yall}{p x q x n array that contain the n indepedent p x q matrices}

\item{maxit}{integer - maximum number of iterations}

\item{df}{numeric - degrees of freedom for the Matrix-variate t}

\item{err}{numeric - tolerance for convergence}

\item{init}{list of initial values with init$mean a p x q mean matrix, init$U p x p row-scale matrix and
init$V qxq column-scale matrix.}
}
\value{
a list with elements\cr\cr
 mean - estimated p x q mean matrix \cr\cr
 U - estimated p x p row-scale matrix \cr\cr
 V - estimated q x q column-scale matrix \cr\cr
 df - degrees of freedom that were used \cr\cr
 iter - iterations it took \cr\cr
 tol - the error that was used to assess convergence  \cr\cr
 logLik - loglikelihood of the final fitted model \cr\cr
 covergence - T or F indicating convergence \cr\cr
 allconv - vector containing all the sum of relative norms, which were used to assess convergence \cr\cr
}
\description{
This function fits the matrix-variate-t distribution with pre-specified degrees of freedom, and under identical
column matrix and AR(1) correlation in the row-scale matrix
}
\details{
This function is not as versatile as the one in MixMatrix but it works well with AR(1).
Will opt for MixMatrix in the future when it gets fixed
}
\examples{
set.seed(1234)
p <- 2
q <- 6
U <- drop(rWishart(1,p,diag(p)))
V <- toeplitz(0.2^(0:(q-1)))
m <- rgamma(p,2) \%o\% rep(1,q)
df <- 5
Y <- MixMatrix::rmatrixt(1000,df,mean = m,U=U,V=V)
fit <- MLmatrixt_ar1(Y,df = df)
list("true" = U , "est" = fit$U)
sapply(list("true" = V , "est" = fit$V),function(x) x[2,1])
sapply(list("true" = m , "est" = fit$mean),function(x) x[,1])
plot(ts(fit$allconv))

}
\references{
\url{https://doi.org/10.1080/10618600.2019.1696208}
}
\author{
Carlos Llosa-Vite
}
