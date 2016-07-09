plusminus.loo <- function (X, Y, method = "plusminus", n_cores, ...) 
{
  Y <- as.matrix(Y)
  nobj <- dim(X)[1]
  npred <- dim(X)[2]
  nresp <- dim(Y)[2]
  dnX <- dimnames(X)[[2]]
  dnY <- dimnames(Y)
  Ymean <- mean(Y)
  method <- match.arg(method, "plusminus")
  fitFunc <- switch(method, plusminus = plusminus.fit)
  fit.all <- fitFunc(X, Y)
  LOOs <- nrow(X)
  Segments <- llply(1:LOOs, function(x) (1:nrow(X))[-x])
  plusminuslooSeg <- function(n.seg) {
    seg <- Segments[[n.seg]]
    Xtrain <- X[seg, ]
    fit <- fitFunc(Xtrain, Y[seg, ])
    Xtest <- X[-seg, ]
    pred <- matrix(0, 1)
    pred[,1]<-sign(Xtest%*%fit$coefficients)
    err<-as.numeric(pred != Y[-seg, ])
    return(list(Predicted = pred,Error=err))
  }
  cl <- makeCluster(getOption("cl.cores", n_cores))
  clusterExport(cl, 
                varlist = c("X", "fitFunc", "Y", "Segments"), 
                envir = environment())
  results <- (parLapply(cl, 1:LOOs, plusminuslooSeg))
  stopCluster(cl)
  cvError <- apply(do.call("rbind", llply(1:LOOs, function(x) results[[x]]$Error)), 2, 
                   function(x) sum(x, na.rm = T))/LOOs
  Predicted <- apply(do.call("rbind", llply(1:LOOs, function(x) results[[x]]$Predicted)), 2, 
                     function(x) sum(x, na.rm = T))
  
  loo.results <- list(cvError = cvError,in.bag = Segments)
  loo.results
}