% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers.R
\name{outliers_cap_iqr}
\alias{outliers_cap_iqr}
\title{cap the outliers using DescTools::Winsorize}
\usage{
outliers_cap_iqr(df, ignore = c("y", "id"))
}
\arguments{
\item{df}{a data.frame with possible outliers to be capped}

\item{ignore}{character vector of ignored variable in imputation}
}
\description{
cap the outliers using DescTools::Winsorize
}
\examples{
x <- airquality
x[1,1] <- 1e06
head(outliers_cap_iqr(x))
}
