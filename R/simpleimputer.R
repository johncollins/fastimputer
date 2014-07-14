#' simple.imputer
#'
#' I use really simple models to predict missing values.
#' For continuous variables I use linear regression and 
#' for categorical variables I use logistic regression.
#' @param df Data frame for all data including missing and non-missing values
#' @keywords impute, imputation, imputer
#' @export
#' @examples
#' df <- data.frame(A=c(1,2,3,1,2), B=as.factor(c(1,2,1,3,NA)), C=c(1.1, 3.5, NA, 3, NA))
#' df.imputed <- simple.imputer(df)

simple.imputer <- function(df) {
  require(nnet)
  # break into imputable and non-inputable names
  df.names <- names(df)
  imputable.names <- df.names[apply(df, 2, function(x)any(which(is.na(x))))]
  imputor.names <- setdiff(df.names, imputable.names)         
  # now train models for and predict missing values by column
  for (name in imputable.names) {
    train.ids <- which(!is.na(df[name]))
    test.ids <- which(is.na(df[name]))
    formula <- paste(name, '~', paste(imputor.names, collapse=' + '))
    fit.msg <- capture.output(suppressMessages(
      if (is.factor(df[[name]])) {
        fit <- multinom(eval(formula), data=df)
      } else {
        fit <- lm(eval(formula), data=df)
      }
    ))
    df[test.ids, name] <- predict(fit, subset(df, subset = 1:nrow(df) %in% test.ids, select = names(df) %in% imputor.names))
  }
  df
}

# test that both numeric and factor vars are processed  correctly
X <- data.frame(A=c(1,2,3,1,2), B=as.factor(c(1,2,1,3,NA)), C=c(1.1, 3.5, NA, 3, NA))

# add pre-analyze functionality which decides what type of imputation 
# is reasonable and how long it will take to run that imputation

# add post-analyze functionality which builds roc curve and returns
# 1. AUC, 2. Confusion matrix, 3. prediction accuracy
