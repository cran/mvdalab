plot.mvdapca <- function(x, ...) {
  object <- x
  if(length(x$D) == 1) {
    print("For a one-component model we don't generate this graph")
  } else {
  df <- data.frame(ncomp = 1:length(object$CV) - 1, CV = object$CV)
  print(with(df, ggplot(df, aes_string(x = ncomp, y = CV)) +
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               geom_line(lwd = 1.25) +
               ggtitle("ekf Cross-Validation") +
               xlab("Ncomps") +
               ylab("PRESS") +
               theme(legend.position = "none") +
               theme(plot.title = element_text(size = 20)) +
               theme(axis.title.x = element_text(size = 15)) +
               theme(strip.text.x = element_text(size = 10, colour = "black", face="bold")) +
               theme(strip.text.y = element_text(size = 10, colour = "black", face="bold")) +
               theme(axis.title.y = element_text(size = 15, angle = 90)) +
               theme(axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5, face = "bold")) +
               theme(axis.text.y = element_text(size = 10, angle = 0, face = "bold")) +
               scale_x_continuous(breaks = df$ncomp)))
  }
}
