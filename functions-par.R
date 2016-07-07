#
# inner functions, modified for parallel execution. 
# Includes the main modification in hat_values.ivreg
#
# place this script at a folder named ~/rdd_parallel
# 
# Auxiliary resource for paper:
# Brazil's Bolsa Familia and Young Adult Workers: A Parallel RDD Approach to Large Datasets

# Original function is hatvalues.ivreg() from package AER
hatvalues.ivreg_par <-function (model, ...) 
{
  xz <- model.matrix(model, component = "projected")
  x <- model.matrix(model, component = "regressors")
  z <- model.matrix(model, component = "instruments")
  solve_qr <- function(x) chol2inv(qr.R(qr(x)))
  
  #Original:
  #diag(x %*% solve_qr(xz) %*% t(x) %*% z %*% solve_qr(z) %*% t(z))
  
  #Parallel:
  sqr_xz <- solve_qr(xz)
  sqr_z  <- solve_qr(z)
  t_x    <- t(x)
  t_z    <- t(z)
  
  nc <- getDoParWorkers()   #Get the number of cores avaliable
  idx <- splitIndices(nrow(x), nc) #Split the first matrix
  
  diagP <- foreach(i = 1:nc, .combine=c) %dopar% { #Parallel loop
      diag(x[idx[[i]],]%*% sqr_xz %*% t_x %*% z %*% sqr_z %*% t_z[,idx[[i]]])
  }
  diagP
  #End parallel

}


# Original Function vcovHC() from package AER
vcovHC_par <- function(x, 
                               type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5"),
                               omega = NULL, sandwich = TRUE,cl=NULL, ...)
{
  type <- match.arg(type)
  rval <- meatHC_par(x, type = type, omega = omega)
  if(sandwich) rval <- sandwich(x, meat = rval, ...)
  return(rval)
}


# Original Function meatHC() from package sandwich
meatHC_par <- function(x, 
                       type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5"),
                       omega = NULL)
{
  ## ensure that NAs are omitted
  if(is.list(x) && !is.null(x$na.action)) class(x$na.action) <- "omit"
  
  ## extract X
  X <- model.matrix(x)
  if(any(alias <- is.na(coef(x)))) X <- X[, !alias, drop = FALSE]
  attr(X, "assign") <- NULL
  n <- NROW(X)
  
  ## get hat values and residual degrees of freedom
  
  ##############################################################
  #Code Change to use new optmized version of hatvalues.ivreg 
  #diaghat <- try(hatvalues.ivreg(x), silent = TRUE)
  diaghat <- hatvalues.ivreg_par(x)
  ##############################################################
  
  df <- n - NCOL(X)
  
  ## the following might work, but "intercept" is also claimed for "coxph"
  ## res <- if(attr(terms(x), "intercept") > 0) estfun(x)[,1] else rowMeans(estfun(x)/X, na.rm = TRUE)
  ## hence better rely on
  ef <- estfun(x)
  res <- rowMeans(ef/X, na.rm = TRUE)
  ## handle rows with just zeros
  res[apply(abs(ef) < .Machine$double.eps, 1L, all)] <- 0
  
  ## if omega not specified, set up using type
  if(is.null(omega)) {
    type <- match.arg(type)
    if(type == "HC") type <- "HC0"
    switch(type,
           "const" = { omega <- function(residuals, diaghat, df) rep(1, length(residuals)) * sum(residuals^2)/df },
           "HC0"   = { omega <- function(residuals, diaghat, df) residuals^2 },
           "HC1"   = { omega <- function(residuals, diaghat, df) residuals^2 * length(residuals)/df },
           "HC2"   = { omega <- function(residuals, diaghat, df) residuals^2 / (1 - diaghat) },
           "HC3"   = { omega <- function(residuals, diaghat, df) residuals^2 / (1 - diaghat)^2 },
           "HC4"   = { omega <- function(residuals, diaghat, df) {
             n <- length(residuals)
             p <- as.integer(round(sum(diaghat),  digits = 0))
             delta <- pmin(4, n * diaghat/p)
             residuals^2 / (1 - diaghat)^delta
           }},
           "HC4m"  = { omega <- function(residuals, diaghat, df) {
             gamma <- c(1.0, 1.5) ## as recommended by Cribari-Neto & Da Silva
             n <- length(residuals)
             p <- as.integer(round(sum(diaghat),  digits = 0))
             delta <- pmin(gamma[1], n * diaghat/p) + pmin(gamma[2], n * diaghat/p)
             residuals^2 / (1 - diaghat)^delta
           }},
           "HC5"   = { omega <- function(residuals, diaghat, df) {
             k <- 0.7 ## as recommended by Cribari-Neto et al.
             n <- length(residuals)
             p <- as.integer(round(sum(diaghat),  digits = 0))
             delta <- pmin(n * diaghat/p, pmax(4, n * k * max(diaghat)/p))
             residuals^2 / sqrt((1 - diaghat)^delta)
           }}
    )
  }
  
  ## process omega
  if(is.function(omega)) omega <- omega(res, diaghat, df)
  rval <- sqrt(omega) * X
  rval <- crossprod(rval)/n
  
  return(rval)
}
