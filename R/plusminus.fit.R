plusminus.fit <- function(XX, YY, ...){
  p <- dim(as.matrix(XX))[2]
  YY. <- scale(YY, scale = FALSE)
  XX_scaled <- (scale(XX, center = FALSE, scale = TRUE))
  XX1 <- cbind(YY., XX_scaled)
  lms <- t(XX1[, -1]) %*% YY.
  lms1 <- as.data.frame(lms) / sqrt(p)
  ccc1 <- sign(lms1[, 1])
  Outs <- list(coefficients = ccc1, X = data.frame(XX_scaled), Y = YY)
}