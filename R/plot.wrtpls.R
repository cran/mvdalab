plot.wrtpls <- function(x, comp = 1:object$ncomp, distribution = "log", ...) {
  object <- x
  if(x$method != "wrtpls") {
    stop("this function is only available for 'wrtpls' objects")
  }
  if(distribution == "log") {
    LogNorms <- sapply(comp, function(x) object$wrtpls[[x]]$lognormwprms)
    colnames(LogNorms) <- comp
    LogNorms.m <- melt(LogNorms)
    names(LogNorms.m) <- c("Perm", "LVs", "LogNorms")
    Actual <- object$actual.normwob[comp, ]
    Actual$LVs <- comp
    Quantiles <- melt(apply(LogNorms, 2, function(x) quantile(x, probs = 1 - object$alpha)))
    Quantiles$LVs <- row.names(Quantiles)
    Crits <- data.frame(LVs = comp, melt(object$wrtpls.crit[comp]))
    Actual$LVs_ <- factor(Actual$LVs, levels = as.factor(comp))
    Quantiles$LVs <- row.names(Quantiles)
    Quantiles$LVs <- as.factor(Quantiles$LVs)
    Crits$LVs <- as.factor(Crits$LVs)
    Crits$LVs_ <- factor(Crits$LVs, levels = as.factor(comp))

    graph1 <- with(LogNorms.m, ggplot(LogNorms.m, aes(x = LogNorms)) +
                     theme_bw() +
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                     geom_histogram(aes(fill = ..count..)) +
                     facet_wrap(~LVs, scales = "free") +
                     geom_vline(data = Actual, aes(xintercept = log(value))) +
                     geom_vline(data = Crits, aes(xintercept = log(value)), lty = 2) +
                     geom_vline(data = Quantiles, aes(xintercept = value), lty = 3) +
                     ggtitle("WRTPLS per LV (Natural Log Scale)") +
                     ylab("Count") +
                     xlab("Actual Values") +
                     theme(legend.position = "none") +
                     theme(plot.title = element_text(size = 20)) +
                     theme(axis.title.x = element_text(size = 15)) +
                     theme(axis.title.y = element_text(size = 15, angle = 90)) +
                     theme(strip.text.x = element_text(size = 10, colour = "black", face="bold")) +
                     theme(axis.text.x = element_text(size = 10, angle = 0, face = "bold")) +
                     theme(axis.text.y = element_text(size = 10, angle = 0, face = "bold")))
    print(graph1)
  } else {
    Norms <- sapply(comp, function(x) object$wrtpls[[x]]$normwprms)
    colnames(Norms) <- comp
    Norms.m <- melt(Norms)
    names(Norms.m) <- c("Perm", "LVs", "Norms")
    Actual <- object$actual.normwob[comp, ]
    Actual$LVs <- comp
    Quantiles <- melt(apply(Norms, 2, function(x) quantile(x, probs = 1 - object$alpha)))
    Quantiles$LVs <- row.names(Quantiles)
    Actual$LVs_ <- factor(Actual$LVs, levels = as.factor(comp))
    Quantiles$LVs <- row.names(Quantiles)
    Quantiles$LVs <- as.factor(Quantiles$LVs)

    graph2 <- with(Norms.m, ggplot(Norms.m, aes(x = Norms)) +
                     theme_bw() +
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                     geom_histogram(aes(fill = ..count..)) +
                     facet_wrap(~LVs, scales = "free") +
                     geom_vline(data = Actual, aes(xintercept = (value))) +
                     geom_vline(data = Quantiles, aes(xintercept = value), lty = 3) +
                     ggtitle("WRTPLS per LV (Original Scale)") +
                     ylab("Count") +
                     xlab("Actual Values") +
                     theme(legend.position = "none") +
                     theme(plot.title = element_text(size = 20)) +
                     theme(axis.title.x = element_text(size = 15)) +
                     theme(axis.title.y = element_text(size = 15, angle = 90)) +
                     theme(strip.text.x = element_text(size = 10, colour = "black", face="bold")) +
                     theme(axis.text.x = element_text(size = 10, angle = 0, face = "bold")) +
                     theme(axis.text.y = element_text(size = 10, angle = 0, face = "bold")))
    print(graph2)
  }
}
