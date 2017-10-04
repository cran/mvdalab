pca.nipals <- function(data, ncomps = 1, Iters = 500, start.vec = NULL, tol = 1e-08) {
  ws.ncomps <- Scores.ncomps <- ws.iters <- Scores.iters <- NULL
  loading.space <- scores.space <- list()
  X <- scale(data, center = TRUE, scale = FALSE)
  data.xa. <- X
  if(is.null(start.vec)) {
    y. <- data.xa.[, sample(1:ncol(data.xa.), 1)]
  } else {
    y. <- start.vec
  }
  data.xa. <- as.matrix(X)
  for (i in 1:ncomps) {
    for (a in c(1:Iters)) {
      y. <- y.
      (ws.pre <- as.matrix((t(data.xa.) %*% y.)))
      (Ws.norm <- (crossprod(t(data.xa.) %*% y.)))
      (ws. <- (ws.pre/sqrt(Ws.norm)[1]))
      (Scores <- (data.xa. %*% ws.))
      (Conv <- t(y. - Scores) %*% (y. - Scores))
      ws.iters <- cbind(ws.iters, ws.)
      Scores.iters <- cbind(Scores.iters, Scores)
      loading.space[[i]] <- ws.iters
      scores.space[[i]] <- Scores.iters
      y. <- Scores
      if (Conv < 1e-08) {
        break
      }
      ws.f <- ws.
      Scores.f <- Scores
    }
    data.xa. <- data.xa. - (Scores.f %*% t(ws.f))
    ws.ncomps <- cbind(ws.ncomps, ws.f)
    Scores.ncomps <- cbind(Scores.ncomps, Scores.f)
    Results <- list(Loadings = ws.ncomps, Scores = Scores.ncomps, 
                    Loading.Space = loading.space, Score.Space = scores.space)
  }
  class(Results) <- "npca"
  Results
}
