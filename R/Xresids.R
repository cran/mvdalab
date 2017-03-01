Xresids <- function(object, ncomp = object$ncomp, conf = c(.95, .99), normalized = TRUE,
                           verbose = FALSE) {
  X.residual.matrix <- as.matrix(object$Xdata) -
    (object$scores[ , 1:ncomp] %*% t(object$loadings[ , 1:ncomp]))
  df.Score <- (apply(X.residual.matrix, 1, function(x) sum(x^2)))
  v <- var(df.Score)
  m <- mean(df.Score)
  Mult <- v / (2 * m)
  dfreed <- (2 * m^2) / v
  `Upper A` <- Mult * qchisq(conf[1], dfreed)
  `Upper B` <- Mult * qchisq(conf[2], dfreed)
  df <- data.frame(Seq = 1:length(df.Score), df.Score, `Upper A`, `Upper B`)
  df$`Upper A` <- ifelse(df.Score > `Upper A` & df.Score < `Upper B`, "Out", "in")
  df$`Upper B` <- ifelse(df.Score > `Upper B`, "Out", "in")
  df$`Upper A` <- ifelse(df$`Upper B` == "Out", "Out", df$`Upper A`)
  print(with(df, ggplot(df, aes_string(Seq, df.Score)) +
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               geom_text(aes_string(label = as.factor(Seq)), size = 3) +
               geom_line() +
               geom_hline(yintercept = df$Upper.A, lty = 2) +
               geom_hline(yintercept = df$Upper.B, lty = 1) +
               theme(legend.position = "none") +
               xlab("Batch Sequence") +
               ylab("Residuals") +
               ggtitle(paste("X Residuals Range Plot for", ncomp, "components\nwith", conf[1], "% and", conf[1], "% Limits")) +
               theme(plot.title = element_text(size = 20)) +
               theme(axis.title.x = element_text(size = 15)) +
               theme(axis.title.y = element_text(size = 15, angle = 90)) +
               theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5, face = "bold")) +
               theme(axis.text.y = element_text(size = 10, angle = 0, face = "bold"))))
  names(df)[2:6] <- c("X-Residual",
                      paste(conf[1] * 100, "%", sep = ""),
                      paste(conf[2] * 100, "%", sep = ""),
                      "Out of Lower CI",
                      "Out of Upper CI")
  if(verbose == TRUE) {
    return(df)
  }
}
