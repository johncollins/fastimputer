% Generated by roxygen2 (4.0.1): do not edit by hand
\name{simple.imputer}
\alias{simple.imputer}
\title{simple.imputer}
\usage{
simple.imputer(df)
}
\arguments{
\item{df}{Data frame for all data including missing and non-missing values}
}
\description{
I use really simple models to predict missing values.
For continuous variables I use linear regression and
for categorical variables I use logistic regression.
}
\examples{
df <- data.frame(A=c(1,2,3,1,2), B=as.factor(c(1,2,1,3,NA)), C=c(1.1, 3.5, NA, 3, NA))
df.imputed <- simple.imputer(df)
}
\keyword{imputation,}
\keyword{impute,}
\keyword{imputer}

