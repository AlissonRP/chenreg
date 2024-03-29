% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_chen.R
\name{auto_chen}
\alias{auto_chen}
\title{auto_chen}
\usage{
auto_chen(
  y,
  data,
  metric = "rmse",
  tau = 0.5,
  alpha = 0.05,
  total = NULL,
  info = FALSE
)
}
\arguments{
\item{y}{is the name of the target variable}

\item{data}{data frame, list or environment (or object coercible by
as.data.frame to a data frame)
               containing the variables in the model.}

\item{metric}{metric you are interested in}

\item{tau}{a number that indicates the quantile that you want to fit
the regression}

\item{alpha}{significance coefficient in hypothesis testing}

\item{total}{number of combinations}

\item{info}{if is TRUE will print in console more information}
}
\description{
auto_chen is used to find the best combinations of exogenous variables based
on the metrics that you interested
}
\note{
the default value of `total` is 50, but if 50
is more than the possible number of combinations `(k)`,
then `k` will be used as the total.
}
\examples{
library(chenreg)
data <- simu[, -1]

auto_chen("Y", data, metric = "rmse", total = 3)
auto_chen("Y", data, metric = "bic", info = TRUE)


}
