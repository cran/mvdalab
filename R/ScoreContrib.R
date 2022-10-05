ScoreContrib <- function(object, ncomp = 1:object$ncomp, obs1 = 1, obs2 = NULL) {
  S <- as.matrix(object$scores[, ncomp])
  P <- as.matrix(object$loadings[, ncomp])
  X <- as.matrix(object$Xdata)
  Class <- class(object)
  if(Class == "mvdapca") {
    Ww <- (P %*% solve(t(P) %*% P))
  } else {
    W <- as.matrix(object$weights[, ncomp])
    Ww <- (W %*% solve(t(P) %*% W))
  }
  if(is.null(obs2)) {
    Xrow <- apply(object$Xdata[obs1, ], 2, mean)
  } else {
    Xrow <- apply(object$Xdata[obs1, ], 2, mean) - apply(object$Xdata[obs2, ], 2, mean)
  }
  New.S <- as.numeric(Xrow %*% Ww)
  Mults <- t(sqrt((New.S / sqrt(diag(cov(S))))^2 * t(((Ww)^2))))
  CPper <- as.matrix(Xrow * Mults)
  CPall <- as.matrix(Xrow * apply(Mults, 1, function(x) sqrt(sum(x^2))))
  Contributions <- data.frame(CPper, Overall = CPall)
  names(Contributions)[ncomp] <- ncomp
  Contributions$Variable <- rownames(Contributions)
  row.names(Contributions) <- NULL
  Contributions <- list(score.contribution = Contributions, ncomp = ncomp, obs1 = obs1, obs2 = obs2)
  class(Contributions) <- "cp"
  Contributions
}
