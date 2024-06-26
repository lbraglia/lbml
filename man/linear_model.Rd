% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_model.R
\name{linear_model}
\alias{linear_model}
\title{template for linear model predictor}
\usage{
linear_model(train, test, formula)
}
\arguments{
\item{train}{training dataset}

\item{test}{test dataset}

\item{formula}{lm formula}
}
\description{
template for linear model predictor
}
\examples{
formulas <- list("all variables" = y ~ . - id)
lms <- lm_maker(formulas)
}
