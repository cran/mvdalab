print.mvdapca <- function (x, ...)
{
  nobj <- nrow(x$scores)
  nvars <- ncol(x$Xdata)
  cat("Principal Component Analysis\n")
  cat("\nFit Summary: \nNumber of objects =", nobj, "\nNumber of Variables =",
      nvars)
  cat("\nPercent Variation Explained:\n")
  print(round(x$Percents.Explained, 3))
  cat("\nCross-Validation Results:\n")
  print(data.frame(PRESS = round(x$CV, 3)))
  cat("\nEigenvalues:\n")
  if(length(x$D) == 1) {
    print(round(x$D, 3))
  } else {
    print(round(diag(x$D), 3))
  }
}
