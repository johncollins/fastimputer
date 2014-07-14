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
#' df <- data.frame(A=c(1,2,3,1,2), B=as.factor(c(1,2,1,3,NA)), C=c(1.1, 3.5, NA, 3, NA))
#' preimputation.analysis <- preimputation.analyzer(df)

preimputation.analyzer <- function(df) {
  list(time=list(simple=10), accuracy=list(simple=0.7)) 
}