% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{graphPort}
\alias{graphPort}
\title{graphPort}
\usage{
graphPort(stock, rf)
}
\description{
Plots a portfolio of three stocks, must use stockData function
}
\examples{
stocks <- stockData(c("AAPL", "AKRX", "SPY"), 2009)
graphPort(stock = stocks, rf = .005)
}

