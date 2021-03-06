T2 <- function (object, ncomp = object$ncomp, phase = 1, conf = c(0.95,
                                                            0.99), verbose = FALSE)
{
  Scores <- as.matrix(object$scores[, 1:ncomp])
  T2.Scores <- diag(Scores %*% solve(cov(Scores)) %*% t(Scores))
  N <- nrow(Scores)
  A <- ncol(Scores)
  if (phase == 1) {
    Upper.95 <- ((N - 1)^2/N) * qbeta(conf[1], A/2, (N -
                                                       A - 1)/2)
    Upper.99 <- ((N - 1)^2/N) * qbeta(conf[2], A/2, (N -
                                                       A - 1)/2)
  }
  else {
    Upper.95 <- (A * (N^2 - 1))/(N * (N - A)) * qf(conf[1],
                                                   A, N - A)
    Upper.99 <- (A * (N^2 - 1))/(N * (N - A)) * qf(conf[2],
                                                   A, N - A)
  }
  df <- data.frame(Seq = 1:length(T2.Scores), T2.Scores, Upper.95,
                   Upper.99)
  df$Result.95 <- ifelse(T2.Scores > Upper.95 & T2.Scores <
                           Upper.99, "Out", "in")
  df$Result.99 <- ifelse(T2.Scores > Upper.99, "Out", "in")
  df$Result.95 <- ifelse(df$Result.99 == "Out", "Out", df$Result.95)
  print(with(df, ggplot(df, aes_string(Seq, T2.Scores)) + theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_text(aes_string(label = as.factor(Seq)), size = 3) +
      theme(legend.position = "none") + geom_line() +
      geom_hline(yintercept = df$Upper.95, lty = 2) +
      geom_hline(yintercept = df$Upper.99, lty = 1) +
      xlab("Batch Sequence") +
      ylab("T2") +
      ggtitle(paste("T2 Range Plot for",
                    ncomp, "components\nwith", conf[1] * 100, "%", "and",
                    conf[2] * 100, "% Limits")) +
      theme(plot.title = element_text(size = 20)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(axis.title.y = element_text(size = 15, angle = 90)) +
      theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, face = "bold")) +
      theme(axis.text.y = element_text(size = 10, angle = 0, face = "bold"))))
  names(df)[2:6] <- c("T2",
                      paste(conf[1] * 100, "%", sep = ""),
                      paste(conf[2] * 100, "%", sep = ""),
                      "Out of Lower CI",
                      "Out of Upper CI")
  if(verbose == TRUE) {
    return(df)
  }
}
