summary.plusminus <- function(object, ...) {
  x <- object
  nobj <- nrow(x$X)
    z <- x
    ans <- z[c("call", "terms", "coefficients")]
    class(ans) <- "summary.psfit"
    ans$coefficients <- matrix(NA, 0L, 1L)
    dimnames(ans$coefficients) <- list(NULL, c("Coefficients"))
    est <- z$coefficients
    est <- est
    class(ans) <- "summary.plusminusFit"
    cat("Call:\n\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print((x$coefficients))
    cat("\nFit Summary: \n\nNumber of objects =", nobj)
    cat("\nNumber of predictor variables =", length(attr(x$terms, "term.labels"))) 
    cat("\nMethod:", "plusminus")
    if(x$val.method == "loo") {
      cat("\nLOO cvError = ", (round(x$validation$cvError, digits = 2)))
    } else {
      cat("\nNo Cross-Validation")
    }
  }  
    
    