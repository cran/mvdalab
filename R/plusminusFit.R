plusminusFit <- function(formula, data, subset, na.action,method = "plusminus", 
                         n_cores = 2, 
                         validation = c("loo", "none"), model = TRUE, 
                         x = FALSE, y = FALSE, ...)
{
  ret.x <- x
  ret.y <- y
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 
             0)
  mf <- mf[c(1, m)]
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  method <- match.arg(method, "plusminus") 
  if (method == "model.frame") 
    return(mf)
  mt <- attr(mf, "terms")
  mm <- model.matrix(mt, mf, contrasts = FALSE)
  Y <- model.response(mf, "numeric")
  Y <- Y2 <- as.matrix(Y)
  colnames(Y) <- deparse(formula[[2]])
  X <- X2 <- no.intercept(model.matrix(mt, mf))
  nobj <- dim(X)[1]
  npred <- dim(X)[2]
  if (length(attr(mt, "term.labels")) == 1 && !is.null(colnames(mf[[attr(mt, 
                                                                         "term.labels")]]))) 
    colnames(X) <- sub(attr(mt, "term.labels"), "", colnames(X))
  switch(match.arg(validation),oob = {
    val<-NULL
  }, 
  none = {
    val <- NULL
  }, loo = {
    val <- plusminus.loo(X2, Y2, method = method, n_cores = n_cores,
                       ...)
  })
  start.time <- proc.time()[3]
  fitFunc <- switch(method, plusminus = plusminus.fit)
  z <- fitFunc(XX=X, YY=Y2, ...)
  z$fit.time <- proc.time()[3] - start.time
  class(z)<-"plusminus"
  z$na.action <- attr(mf, "na.action")
  z$val.method <- validation
  z$method <- method
  z$validation <- val
  z$call <- match.call()
  z$terms <- mt
  z$mm <- mm
  if (model) 
    z$model <- mf
  z
}