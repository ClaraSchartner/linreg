linreg <- function(x, ...){
    UseMethod("linreg")
}
  
print.linreg <- function(x, ...){
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(x$coefficients)
}

linreg <- structure(list(), class = "linreg")

#xlab is missing:fitted values 
plot.linreg <- function(x, ...){
    library(ggplot2)
    devAskNewPage(ask = TRUE)
    
    one <- ggplot(data=x$fit.res,aes(x=fit,y=res))+geom_point()+
    geom_smooth(method = "loess", formula = y ~ x,se=FALSE, colour = "red") + 
    xlab(x$call) + ylab("residuals") + ggtitle("Residuals vs. Fitted")
    
    two <- ggplot(data=x$fit.res,aes(x=fit,y=sqrt(sqrt(abs(res)))))+geom_point()+
    geom_smooth(method = "loess", formula = y ~ x,se=FALSE, colour = "red") + 
    xlab(x$call) + ylab(expression(paste(sqrt(abs("Standardized residuals"))))) + ggtitle("Scale Location")
    
    print(one)
    print(two)
    on.exit(devAskNewPage(FALSE))
}

resid.linreg <- function(x, ...){
    return(x$residuals)
}

predict.linreg <- function(x, ...){
   return(x$fitted)
}

coef.linreg <- function(x, ...){
    return(x$coefficients)
}

summary.linreg <- function(x, ...){
    cat("Call:\n")
    print(x$call) 
    cat("\nCoefficients:\n")
    sd_coeff <- sqrt(x$varcoeff)
    ls <- cbind(Estimate = x$coefficients ,Std.Error = sd_coeff, t.value = x$tval, p.value = x$pval)
    print(ls)
    cat("\nResidual standard error:")
    sd_res <- sqrt(x$varres)
    cat(sd_res, "on", x$df, "degree of freedom")
}