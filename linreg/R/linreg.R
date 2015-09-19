
linreg_function <- function(formula, data){
  X <- model.matrix(formula, data)
  y <- as.matrix(data[all.vars(formula)[!(all.vars(formula) %in% colnames(X))]])
  stopifnot(is.numeric(y)&is.numeric(X))
  Q <- qr.Q(qr(X))
  R <- qr.R(qr(X))
  
  ans <- solve(R) %*% t(Q) %*% y
  beta <- as.vector(ans)
  names(beta) <- colnames(X)
  
  #the fitted values 
  fit <- X %*% beta
  fit <- as.vector(fit)
  
  #the residuals
  res <- y - fit
  
  fit.res<-data.frame(fit,res)
  #not working if class is linreg
  # class(fit.res) <- "linreg"
  names(fit.res)<-c("fit","res")
  
  
  
  res <- as.vector(res)
  
  fit.res <- data.frame(fit, res)
  names(fit.res) <- c("fit", "res")
  
  
  #the degree of freedoms
  n <- nrow(X)
  p <- ncol(X)
  df <- n - p
  
  #the residual variance
  var_res <- as.vector((t(res) %*% res) / df)
  
  #the variance of the regression coefficients
  var_tmp <- var_res * solve(t(R) %*% R)
  var_coeff <- numeric()
  for(i in 1:nrow(var_tmp)){
    var_coeff <- c(var_coeff, var_tmp[i,i])
  }
  
  #the t-values for each coefficient
  tval <- numeric()
  for(i in 1:length(beta)){
    tval <- c(tval, beta[i] / sqrt(var_coeff[i]))
  }
  
  #p-value
  pval <- 2*pt(-abs(tval), df)
  
  a <- list(coefficients = beta, fitted = fit, residuals = res, varres = var_res, varcoeff = var_coeff, tvalues = tval, pvalues = pval, df = df, fit.res=fit.res)
  a$call <- match.call()
  class(a) <- "linreg"
  return(a)    
}