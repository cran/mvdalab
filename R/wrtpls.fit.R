wrtpls.fit <- function (X, Y, ncomp, perms, alpha, ...)
{
  Y <- as.matrix(Y)
  dnX <- dimnames(X)[[2]]
  dnY <- dimnames(Y)
  nobj <- dim(X)[1]
  npred <- dim(X)[2]
  nresp <- dim(Y)[2]
  Xmeans <- colMeans(X)
  X <- X - rep(Xmeans, each = nobj)
  Ymean <- mean(Y)
  Y <- Y - Ymean
  A <- ncomp
  W <- matrix(0, ncol(X), A)
  TT <- matrix(0, nrow = nrow(X), ncol = A)
  D2 <- matrix(0, nrow = ncol(X), ncol = A)
  iD2 <- matrix(0, nrow = ncol(X), ncol = A)
  iB <- matrix(0, ncol(X), A)
  iPreds <- matrix(0, nrow(X), A)
  iResids <- matrix(0, nrow(X), A)
  tQ <- matrix(0, nrow = A, ncol = 1)
out <- NULL
    W[, 1] <- crossprod(X, Y)
    W[, 1] <- W[, 1]/sqrt(c(crossprod(W[, 1])))
    TT[, 1] <- X %*% W[, 1]
    D2[1, 1] <- sqrt(c(crossprod(TT[, 1])))
    TT[, 1] <- TT[, 1]/D2[1, 1]
    iD2[1, 1] <- 1/D2[1, 1]
    q1 <- crossprod(TT[, 1], Y)
    tQ[1, ] <- q1
    if (A == 1) {
        out <- list(W, D2[1, 1], TT, iD2[1, 1], tQ)
    } else if (A > 1) {
    for (a in 2:A) {
      W[, a] <- crossprod(X, TT[, a - 1]) - D2[a - 1, a -
                                                 1] * W[, a - 1]
      W[, a] <- W[, a] - W[, 1:(a - 1)] %*% crossprod(W[,
                                                        1:(a - 1)], W[, a])
      D2[a - 1, a] <- -sqrt(c(crossprod(W[, a])))
      W[, a] <- W[, a]/D2[a - 1, a]
      TT[, a] <- (X %*% W[, a]) - D2[a - 1, a] * TT[, a -
                                                      1]
      TT[, a] <- TT[, a] - TT[, 1:(a - 1)] %*% crossprod(TT[,
                                                            1:(a - 1)], TT[, a])
      D2[a, a] <- sqrt(c(crossprod(TT[, a])))
      TT[, a] <- TT[, a]/D2[a, a]
      iD2[a, a] <- 1/D2[a, a]
      iD2[1:(a - 1), a] <- iD2[1:(a - 1), a - 1] * (-D2[a -
                                                          1, a]/D2[a, a])
      q1 <- crossprod(TT[, a], Y)
      tQ[a, ] <- q1
      out <- list(W, D2[1:A, 1:A], TT, iD2[1:A, 1:A], tQ)
    }
    }
    W <- out[[1]]
    D2 <- as.matrix(out[[2]])
    TT <- out[[3]]
    iD2 <- as.matrix(out[[4]])
    tQ <- as.matrix(out[[5]])
    P <- crossprod(X, TT)
    D2.b <- D2
    diag(D2.b)[diag(D2.b) < .Machine$double.eps^0.5] <- 1
    for (a in 1:A) {
      P[, a] <- P[, a]/D2.b[a, a]
    }
    R <- W %*% iD2
    if (A == 1) {
      iB[, 1] <- W[, 1] * iD2[1, 1] * tQ[1, 1]
      out.iB <- iB
    } else {
      for (a in 2:A) {
        iB[, 1] <- W[, 1] * iD2[1, 1] * tQ[1, 1]
        iB[, a] <- R[, 1:a] %*% tQ[1:a, 1]
        out.iB <- iB
      }
    }
    Betas <- R %*% tQ
    fitted.pre <- X %*% Betas
    fitted <- fitted.pre + Ymean
    Yactual <- Y + Ymean
    Xdata <- as.data.frame(X)
    yloadings <- as.matrix(c(tQ)/diag(D2))
    if (A == 1) {
      iPreds[, 1] <- X %*% W[, 1] * iD2[1, 1] * tQ[1, 1] +
        Ymean
      out.iPreds <- iPreds
    } else {
      for (a in 2:A) {
        iPreds[, 1] <- X %*% W[, 1] * iD2[1, 1] * tQ[1,
                                                     1] + Ymean
        iPreds[, a] <- X %*% R[, 1:a] %*% tQ[1:a, 1] +
          Ymean
        out.iPreds <- iPreds
      }
    }
    if (A == 1) {
      iResids[, 1] <- Y - (X %*% out.iB[, 1])
      out.iResids <- iResids
    } else {
      for (a in 2:A) {
        iResids[, 1] <- Y - (X %*% out.iB[, 1])
        iResids[, a] <- Y - (X %*% out.iB)[, a]
        out.iResids <- iResids
      }
    }
    colnames(out.iB) <- paste("ncomp", 1:A, sep = ".")
    colnames(out.iPreds) <- paste("ncomp", 1:A, sep = ".")
    colnames(out.iResids) <- paste("ncomp", 1:A, sep = ".")
    colnames(R) <- paste("ncomp", 1:A, sep = ".")
    class(TT) <- "scores"
    class(P) <- class(q1) <- class(yloadings) <- "loadings"
    class(W) <- "weights"
    class(out.iPreds) <- "predict"
    class(out.iB) <- "coefficients"
    class(Xmeans) <- "vector"
    row.names(P) <- dnX
    row.names(W) <- dnX
    row.names(out.iB) <- dnX
    row.names(Betas) <- dnX
    if (A == 1){
    scores <- TT * (diag(D2))
    } else {
      scores <- TT %*% diag(diag(D2))
    }
    alpha <- alpha
    permuts <- perms
    perm.index =  sapply(1:perms,function(x){sample(c(1:nobj))})
    v.. <- v..perm <- matrix(0, nrow = A, ncol = 1)
    D2. <- D2
    xdef <- as.matrix(X)
    ydef <- Y
    UTdef <-  abs(D2.[upper.tri(D2.)])[abs(D2.[upper.tri(D2.)]) > 0]
    W1def <- crossprod(xdef, ydef)
    bs <- c(sqrt(crossprod(crossprod(xdef, ydef))), UTdef)
    v..[1, 1] <- bs[1]
    ds <- diag(D2.)
    if(A > 1) {
      for(a in 2:A) {
        v..[a, 1] <- ((bs[a] * v..[a -1, 1]) / ds[a - 1])
      }
    } else v..
   normwobs <- data.frame(LVs = 1:A, Var2 = NA, value = sqrt(v..^2))
    ydefs.pre <- sapply(1:A, function(x) {
      as.matrix(scores[, x] %*% t(yloadings[x, 1]))
    })
    ydefs <- matrix(NA, nrow = nrow(X), ncol = A + 1)
    ydefs[, 1] <- Y
    for(a in 2:(A + 1)) {
      ydefs[, a] <- ydefs[, a - 1] - ydefs.pre[, a - 1]
    }
    xdefs <- lapply(1:A, function(x) {
      as.matrix(scores[, x:A] %*% t(P[, x:A]))
    })
    wrtpls.out <- lapply(1:A, function(x) {
      yprms <- matrix(ydefs[, x][perm.index], nrow = nobj, byrow = FALSE )
      wprms <- t(xdefs[[x]]) %*% yprms
      normwprms = t(sqrt(colSums(wprms^2)))
      lognormwprms = log(normwprms)
      mlognormwprms = mean(lognormwprms)
      stdlognormwprms = sd(lognormwprms)
      list(yprms = yprms, wprms = wprms, normwprms = normwprms,
           lognormwprms = lognormwprms, mlognormwprms = mlognormwprms,
           stdlognormwprms = stdlognormwprms)
    })
    tcri <- qt(1 - .05, nobj - 1)
    mlognormwprms.. <- sapply(1:A, function(x) wrtpls.out[[x]]$mlognormwprms)
    stdlognormwprms.. <- sapply(1:A, function(x) wrtpls.out[[x]]$stdlognormwprms)
    crit <- exp(mlognormwprms.. + tcri * stdlognormwprms..)
    wrtpls.out.Sig <- sapply(1:length(crit), function(x) normwobs$value[x] > crit[x])
    Outs <- list(loadings = P, weights = W, D2 = D2, iD2 = iD2, Ymean = Ymean,
         Xmeans = Xmeans, coefficients = out.iB, y.loadings = yloadings,
         scores = scores, R = R, Y = Y, Yactual = Yactual,
         fitted = fitted, residuals = out.iResids, Xdata = Xdata,
         iPreds = out.iPreds, y.loadings2 = tQ,
         wrtpls = wrtpls.out, wrtpls.out.Sig = wrtpls.out.Sig, wrtpls.crit = crit,
         actual.normwobs = normwobs)
}
