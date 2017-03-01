PE <- function (object, verbose = FALSE) {
  if (object$method == "bidiagpls" | object$method == "wrtpls") {
    if(object$ncomp == 1) {
      print("For a one-component model we don't generate this graph")
    } else {
  R2X.individual <- sapply(1:object$ncomp, function(x) {
    1 - sum(diag(crossprod(as.matrix(object$Xdata) -
            (object$scores[, x] %*% t(object$loadings[, x])))))/sum(diag(crossprod(as.matrix(object$Xdata))))
  })
  Ind <- data.frame(Percentages = R2X.individual * 100,
         Type = "Individual.Percent", Component = 1:object$ncomp)
  Cumulative <- data.frame(Percentages = cumsum(R2X.individual *
         100), Type = "Cumulative.Percent", Component = 1:object$ncomp)
  df <- rbind(Ind, Cumulative)
  df$Type <- factor(df$Type, levels(df$Type)[c(1, 2)])
  print(with(df, ggplot(df, aes_string(x = Component, y = Percentages)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_line(lwd = 1.25) +
      facet_wrap(~Type, scales = "free") +
      ggtitle("Percent Explained of X") +
      ylab("% Variance Explaned") +
      xlab("Latent Variable Number") +
      theme(legend.position = "none") +
      theme(plot.title = element_text(size = 20)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(strip.text.x = element_text(size = 10, colour = "black", face = "bold")) +
      theme(strip.text.y = element_text(size = 10, colour = "black", face = "bold")) +
      theme(axis.title.y = element_text(size = 15, angle = 90)) +
      theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5, face = "bold")) +
      theme(axis.text.y = element_text(size = 10, angle = 0, face = "bold")) +
      scale_x_continuous(breaks = 1:object$ncomp)))
    if(verbose == TRUE) {
      return(df)
    }
    }
  }
  else {
  if(length(object$D) == 1) {
    print("For a one-component model we don't generate this graph")
  } else {
  df <- data.frame(stack(object$Percents.Explained[, 1:2]),
        object$Percents.Explained[, 3])
  names(df) <- c("Percentages", "Type", "Component")
  df$Type <- factor(df$Type, levels(df$Type)[c(2, 1)])
  print(with(df, ggplot(df, aes_string(x = Component, y = Percentages)) +
      theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_line(lwd = 1.25) +
      facet_wrap(~Type, scales = "free") +
      ggtitle("Percent Explained of X") +
      ylab("% Variance Explaned") +
      xlab("Latent Variable Number") +
      theme(legend.position = "none") +
      theme(plot.title = element_text(size = 20)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(strip.text.x = element_text(size = 10, colour = "black", face = "bold")) +
      theme(strip.text.y = element_text(size = 10, colour = "black", face = "bold")) +
      theme(axis.title.y = element_text(size = 15, angle = 90)) +
      theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5, face = "bold")) +
      theme(axis.text.y = element_text(size = 10, angle = 0, face = "bold")) +
      scale_x_continuous(breaks = object$Percents.Explained[, 3])))
    if(verbose == TRUE) {
      return(df)
    }
    }
  }
}
