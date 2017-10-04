print.mvdareg <- function (x, ...) 
{
  object <- x
  switch(object$method, bidiagpls = {
    ana = "Partial least squares regression"
    alg = "bidiag"
  }, wrtpls = {
    ana = "Partial least squares regression"
    alg = "wrtpls"
  }, stop("Unknown fit method."))
  cat(ana, "fitted with the", alg, "algorithm")
  # if (!is.null(object$validation)) 
  #   
  #   cat("\nCross-validated using", object$validation$Bootstraps, 
  #       "bootstrap samples")
  cat("\nCall:\n", deparse(object$call, width.cutoff = 500), 
      "\n", sep = "")
  invisible(object)
}