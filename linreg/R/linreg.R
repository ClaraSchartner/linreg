

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
##Test outside the function with faithful data
X <- model.matrix(eruptions~waiting, data = faithful)
y <- as.matrix(faithful[,1, drop = FALSE])

#xlab is missing:fitted values 
# http://stackoverflow.com/questions/13223846/ggplot2-two-line-label-with-expression
#to make 2 lines
plot.linreg <- function(x, ...){

ggplot(data=x$fit.res,aes(x=fit,y=res))+geom_point()+
    geom_smooth(method = "loess", formula = y ~ x,se=FALSE, colour = "red") + 
    xlab(x$call) + ylab("residuals") + ggtitle("Residuals vs. Fitted")
 ggplot(data=x$fit.res,aes(x=fit,y=sqrt(abs(res))))+geom_point()+
   geom_smooth(method = "loess", formula = y ~ x,se=FALSE, colour = "red") + 
 xlab(x$call) + ylab(expression(paste(sqrt("Standardized residuals")))) + ggtitle("Scale−Location")

 }

resid.linreg <- function(x, ...){
    return(res)
}

#???
pred.linreg <- function(x, ...){
    return(fit)
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





