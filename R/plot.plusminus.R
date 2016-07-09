plot.plusminus <- function(x, ncomp = 2, comps = c(1, 2), ...)
{
  X <- x$X
  if(ncol(X) == 1){
    stop("You can't do PCA on one variable")
  }
  if (is.null(ncomp)) {
    if (nrow(X) <= ncol(X)) {
      ncomp <- nrow(X)
    }
    else ncomp <- ncol(X)
  }
  pc1 <- pcaFit(X, ncomp = ncomp)
  pcs <- data.frame(A = pc1$scores[, comps[1]], B = pc1$scores[, comps[2]])
  pcs$`Decision Values` <- as.matrix(X) %*% x$coefficients
  pcs$Class <- ifelse(pcs$`Decision Values` > 1, "Class A", "Class B")
  pcs$Actual <- x$Y
  output.graph <- with(pcs, ggplot(pcs, aes(x = A, y = B)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_point(aes(col = Class, pch = Class), size = 5) + 
    geom_vline(xintercept = 0, col = "lightgrey") + 
    geom_hline(yintercept = 0, col = "lightgrey") + 
      ylab(paste("PC", comps[2])) + 
      xlab(paste("PC", comps[1])) + 
    ggtitle("plusminusFit Score Plot") + 
    theme(plot.title = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(axis.title.y = element_text(size = 20, angle = 90)) +
    theme(axis.text.x = element_text(size = 20, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 20, angle = 0, face = "bold")))
  print(output.graph)
}




