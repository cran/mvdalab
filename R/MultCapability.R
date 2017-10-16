MultCapability <- function(data, lsls, usls, targets,
                           ncomps = NULL, Target = FALSE) {
  X <- as.matrix(data)
  m <- nrow(X)
  ColMeans <- colMeans(X)
  ColSD <- sqrt(colSums((X - rep(colMeans(X), each = m))^2)/(m - 1))
  SVD <- svd(cov(X), nu = ncomps, nv = ncomps)
  eigenValues <- SVD$d[1:ncomps]
  eigenValuesSum <- sum(eigenValues)
  rightsvs <- SVD$v
  ncomp.inv <- 1 / ncomps
  mult.3 <- 3
  mult.6 <- 6
  
  projectedSpecs <- t(rightsvs) %*% cbind(lsls, usls, targets, ColMeans)
  colnames(projectedSpecs) <- c("lslspc", "uslspc", "targetspc", "colmeanspc")
  cpI <- abs((projectedSpecs[, "uslspc"] - projectedSpecs[, "lslspc"])) / (mult.6 * sqrt(eigenValues)) 
  cpkIa <- abs((projectedSpecs[, "lslspc"] - projectedSpecs[, "colmeanspc"])) / (mult.3 * sqrt(eigenValues)) 
  cpkIb <- abs((projectedSpecs[, "colmeanspc"] - projectedSpecs[, "uslspc"])) / (mult.3 * sqrt(eigenValues)) 
  cpkIs1 <- cbind(cpkIa, cpkIb)
  cpkIa2 <- abs((projectedSpecs[, "lslspc"] - projectedSpecs[, "colmeanspc"])) / (mult.3 * sqrt(eigenValues + (projectedSpecs[, "colmeanspc"] - projectedSpecs[, "targetspc"])^2)) 
  cpkIb2 <- abs((projectedSpecs[, "colmeanspc"] - projectedSpecs[, "uslspc"])) / (mult.3 * sqrt(eigenValues + (projectedSpecs[, "colmeanspc"] - projectedSpecs[, "targetspc"])^2)) 
  cpkIs2 <- cbind(cpkIa2, cpkIb2)
  
  mcp_wang <- prod(cpI)^(1/ncomps)
  mcpk_wang <- prod(apply(cpkIs1, 1, min))^ncomp.inv
  mcpm_wang <- prod((abs((projectedSpecs[, "uslspc"] - projectedSpecs[, "lslspc"])) / (mult.6 * sqrt(eigenValues + (projectedSpecs[, "colmeanspc"] - projectedSpecs[, "targetspc"])^2))))^ncomp.inv
  mcpmk_wang <- prod(apply(cpkIs2, 1, min))^ncomp.inv
  Wang1 <- c(ncomps = ncomps, mcp_wang = mcp_wang, mcpk_wang = mcpk_wang, 
             mcpm_wang = mcpm_wang, mcpmk_wang = mcpmk_wang)
  Wang <- data.frame(Index = names(Wang1), Metrix = Wang1)
  row.names(Wang) <- NULL
  
  spaceDiff_xe <- as.vector(cpI * eigenValues) 
  mcp_xe <- (sum(spaceDiff_xe)) / eigenValuesSum 
  spaceDiff_xe.min <- apply(cpkIs1, 1, min) * eigenValues   
  mcpk_xe <- (sum(spaceDiff_xe.min)) / eigenValuesSum 
  spaceDiff_xe.normed <- (abs(projectedSpecs[, "uslspc"] - projectedSpecs[, "lslspc"]) / (mult.6 * sqrt(eigenValues + (projectedSpecs[, "colmeanspc"] - projectedSpecs[, "targetspc"])^2)))  * eigenValues 
  mcpm_xe <- (sum(spaceDiff_xe.normed)) / eigenValuesSum 
  spaceDiff_xe.normed.min <- apply(cpkIs2, 1, min) * eigenValues 
  mcpmk_xe <- (sum(spaceDiff_xe.normed.min)) / eigenValuesSum
  Xekalaki1 <- c(ncomps = ncomps, mcp_xe = mcp_xe, mcpk_xe = mcpk_xe, 
                 mcpm_xe = mcpm_xe, mcpmk_xe = mcpmk_xe)
  Xekalaki <- data.frame(Index = names(Xekalaki1), Metrix = Xekalaki1)
  row.names(Xekalaki) <- NULL
  
  spaceDiff_wang2 <- as.vector(cpI^eigenValues) 
  mcp_wang_2 <- (prod(spaceDiff_wang2))^(1 / eigenValuesSum)
  spaceDiff_wang2.min <- apply(cpkIs1, 1, min)^eigenValues   
  mcpk_wang_2 <- (prod(spaceDiff_wang2.min))^(1 / eigenValuesSum)
  spaceDiff_wang2.normed <- (abs(projectedSpecs[, "uslspc"] - projectedSpecs[, "lslspc"]) / (mult.6 * sqrt(eigenValues + (projectedSpecs[, "colmeanspc"] - projectedSpecs[, "targetspc"])^2)))^ eigenValues 
  mcpm_wang_2 <- (prod(spaceDiff_wang2.normed))^(1 / eigenValuesSum)
  spaceDiff_wang2.normed.min <- apply(cpkIs2, 1, min)^eigenValues 
  mcpmk_wang_2 <- (prod(spaceDiff_wang2.normed.min))^(1 / eigenValuesSum)
  Wang21 <- c(ncomps = ncomps, mcp_wang_2 = mcp_wang_2, mcpk_wang_2 = mcpk_wang_2, 
              mcpm_wang_2 = mcpm_wang_2, mcpmk_wang_2 = mcpmk_wang_2)
  Wang2 <- data.frame(Index = names(Wang21), Metrix = Wang21)
  row.names(Wang2) <- NULL
  
  if(Target == TRUE) {
    Pre.Ppk1 <- cbind((targets - lsls) / (mult.3 * ColSD),
                      (usls - targets) / (mult.3 * ColSD))
    Ppk <- data.frame(Index = "Ppk", `Individual Ppks` = apply(Pre.Ppk1, 1, min))
    row.names(Ppk) <- colnames(X)
  } else {
    Pre.Ppk2 <- cbind((ColMeans - lsls) / (mult.3 * ColSD),
                      (usls - ColMeans) / (mult.3 * ColSD))
    Ppk <- data.frame(Index = "Ppk", `Individual Ppks` = apply(Pre.Ppk2, 1, min))
    row.names(Ppk) <- colnames(X)
  }
  Results <- list("multivariate capability indices - Wang CP" = Wang, 
                  "multivariate capability indices - Xekalaki CP" = Xekalaki, 
                  "multivariate capability indices - Wang2 CP" = Wang2, 
                  "Individual Parameter Ppks" = Ppk)
  class(Results) <- "mcpk"
  Results
}