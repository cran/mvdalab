plsFit <- function(formula, data, subset, ncomp = NULL, na.action, method = c("bidiagpls", "wrtpls"), 
                       scale = TRUE, n_cores = 2, alpha = 0.05, perms = 2000, 
          validation = c("none", "oob", "loo"), boots = 1000, model = TRUE, parallel = FALSE,
          x = FALSE, y = FALSE, ...) 
{
  if (!is.logical(scale) || length(scale) != 1) 
    stop("'scale' must be 'TRUE' or 'FALSE'")
  if (n_cores > 4) {
    stop("Number of cores is limited to 4")
  }
  options(contrasts = c("contr.niets", "contr.poly"))
  ret.x <- x
  ret.y <- y
  mf <- match.call(expand.dots = FALSE)
  scaled <- mf$scale
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 
             0)
  mf <- mf[c(1, m)]
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  method <- match.arg(method, c("bidiagpls", "wrtpls"))
  if (method == "model.frame") 
    return(mf)
  mt <- attr(mf, "terms")
  mm <- model.matrix(mt, mf)
  Y <- model.response(mf, "numeric")
  Y <- as.matrix(Y)
  Y2 <- as.matrix(Y)
  colnames(Y) <- deparse(formula[[2]])
  X <- X2 <- no.intercept(model.matrix(mt, mf))
  nobj <- dim(X)[1]
  npred <- dim(X)[2]
  if (length(attr(mt, "term.labels")) == 1 && !is.null(colnames(mf[[attr(mt, 
                                                                         "term.labels")]]))) 
    colnames(X) <- sub(attr(mt, "term.labels"), "", colnames(X))
  if (missing(ncomp) | is.null(ncomp)) {
    ncomp <- min(nobj - 1, npred)
  }
  else {
    if (ncomp < 1 || ncomp > min(nobj - 1, npred)) 
      stop("Invalid number of components, ncomp")
    ncompWarn <- TRUE
  }
  sdscale <- identical(TRUE, scale)
  if (!(is.numeric(scale))) 
    sdscales <- sqrt(colSums((X - rep(colMeans(X), each = nobj))^2)/(nobj - 
                                                                       1))
  if (is.numeric(scale)) 
    if (length(scale) == npred) 
      X <- X/rep(scale, each = nobj)
  else stop("length of 'scale' must equal the number of x variables")
  validation <- ifelse(method == "bidiagpls" & validation == 
                         "oob", "oob", ifelse(method == "bidiagpls" & validation == 
                                                "loo", "loo", ifelse(method == "wrtpls", "none", "none")))
  switch(match.arg(validation), oob = {
    val <- mvdaboot(X2, Y2, ncomp, method = method, boots = boots, 
                    n_cores = n_cores, scale = sdscale, parallel = parallel, ...)
  }, none = {
    val <- NULL
    ncomp <- ncomp <- min(nobj - 1, npred)
  }, loo = {
    val <- mvdaloo(X2, Y2, ncomp, method = method, scale = sdscale, 
                   ...)
  })
  if (identical(TRUE, ncomp > val$ncomp)) {
    ncomp <- val$ncomp
    if (ncompWarn) 
      warning("`ncomp' reduced to ", ncomp, " due to cross-validation")
  }
  fitFunc <- switch(method, bidiagpls = bidiagpls.fit, wrtpls = wrtpls.fit)
  if (sdscale == TRUE) {
    scale <- sqrt(colSums((X - rep(colMeans(X), each = nobj))^2)/(nobj - 
                                                                    1))
    if (any(abs(scale) < .Machine$double.eps^0.5)) 
      warning("Scaling with (near) zero standard deviation")
    X <- X/rep(scale, each = nobj)
  }
  start.time <- proc.time()[3]
  z <- fitFunc(X, Y, ncomp, perms, alpha, ...)
  z$fit.time <- proc.time()[3] - start.time
  class(z) <- "mvdareg"
  z$na.action <- attr(mf, "na.action")
  z$val.method <- validation
  z$ncomp <- ncomp
  if (method == "wrtpls") {
    z$perms <- perms
    z$alpha <- alpha
  }
  else {
    z$perms <- NULL
    z$alpha <- NULL
  }
  z$method <- method
  if (is.numeric(scale)) {
    z$scale <- scale
  }
  else z$scale <- scale
  z$scaled <- scaled
  z$validation <- val
  z$call <- match.call()
  z$terms <- mt
  z$mm <- mm
  if (model) 
    z$model <- mf
  if (ret.x) 
    z$x <- X
  if (ret.y) 
    z$y <- Y
  z
}