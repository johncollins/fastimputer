#' preimputation.analyzer
#'
#' I give a user the option to predict how long using 
#' this module will take and how accurate the results
#' will be by doing imputation on a small set and 
#' extrapolating. Further, I give advice about what
#' kinds of missingness are observed within the data.
#' @param df Data frame for all data including missing and non-missing values
#' @keywords impute, imputation, imputer, missing
#' @export
#' @examples
#' df1 <- data.frame(A=c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3), B=as.factor(c(1,2,1,3,NA,2,1,2,3,NA,1,3,1,2,NA)), C=c(1.1, 3.5, NA, 3, NA, NA, 4.1, 2.2, 2.3, 1.4, 1, 2, 5, 3, 4))
#' preimputation.analysis <- preimputation.analyzer(df1)
#' df2 <- read.table(
#' "http://www.unt.edu/rss/class/Jon/Benchmarks/rrp.ex.data.txt",
#' header = TRUE, sep = ",", na.strings = "NA",
#' dec = ".", strip.white = TRUE)
#' preimputation.analysis <- preimputation.analyzer(df2)

preimputation.analyzer <- function(df) {
  nrows <- nrow(df)
  na.props <- apply(df, 2, function(x)length(which(is.na(x))) / nrows)
  df.small <- df[sample(1:nrows, min(nrows, 5000)), ]
  df.small.full <- df.small[complete.cases(df.small), ]
  # knock out proportionate numbers of items
  df.names <- names(df)
  imputable.names <- df.names[apply(df.small, 2, function(x)any(which(is.na(x))))]
  df.small.full.imputable <- df.small.full
  for (name in imputable.names) {
    n.to.impute <- ceiling(nrow(df.small.full)*na.props[name])
    df.small.full.imputable[sample(1:nrow(df.small.full), n.to.impute), name] <- NA 
  }
  # then impute and measure time and error
  simple.time <- system.time(df.small.full.imputed <- simple.imputer(df.small.full.imputable))[1]
  simple.error <- combined.error(df.small.full, df.small.full.imputed)
  list(time=list(simple=simple.time*(nrows/nrow(df.small.full))), error=list(simple=simple.error))
}

#' combined.error
#'
#' I measure categorical vairable imputation error as absolute error
#' and continuous variable imputation error as mean squared error

combined.error <- function(df1, df2, categorical.inds) {
  categorical.inds <- sapply(names(df1), function(x)is.factor(df1[[x]]))
  cat.err <- categorical.error(df1[categorical.inds], df2[categorical.inds])
  cnt.err <- continuous.error(df1[!categorical.inds], df2[!categorical.inds])
  list(categorical=cat.err, continuous=cnt.err)
}

#' categorical.error
#' 
#' I measure the absolute error of imputing the categorical variables
categorical.error <- function(df1, df2) {
  length(which(df1 != df2)) / (nrow(df1) * ncol(df1))
}

#' I measure the mean squared error of imputing the ontinuous variables
continuous.error <- function(df1, df2) {
  sum((df1 - df2) ^ 2) / (nrow(df1) * ncol(df1))
}