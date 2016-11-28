## limit model set (I think this is 'R' in B&A)?
## limit p/n (>10? >20?)
## try AICcmodavg?

library(MASS)

mkpars <- function(n_true=20,g=0.8) {
    return(g^(0:(n_true-1)))
}
    
##' @param n_true dimensionality of true model
##' @param g geometric decrease in effect size
##' @param pcor correlation among parameters (compound-symmetric)
##' @param stddev standard deviation of response
##' @param seed random-number seed
##' @param N sample size
simfun <- function(N=1000,
                   n_true=20,
                   g=0.8,
                   pars=NULL,
                   pcor=0.3,
                   stddev=2,
                   seed=NULL) {
    require(MASS)
    if (!is.null(seed)) set.seed(seed)

    if (is.null(pars)) pars <- mkpars(n_true,g)
    if (pcor==0) {
        m <- matrix(rnorm(N*n_true),N,n_true)
    } else {
        S <- matrix(pcor,n_true,n_true)
        diag(S) <- 1
        m <- MASS::mvrnorm(N,mu=rep(0,n_true),Sigma=S)
    }
    if (n_true>99) warning("n_true>99; parameters may not be properly sorted")
    dimnames(m) <- list(NULL,sprintf("b%0.2d",1:n_true))
    beta <- c(0,pars)
    mu <- cbind(1,m) %*% beta
    y <- rnorm(N,mu,sd=stddev)
    dd <- data.frame(y,m)
    attr(dd,"true_pars") <- pars
    return(dd)
}

fitfun <- function(data, n_full=10, subset=NULL,
                   method=c("dredge","full")) {
    method <- match.arg(method)
    ## 1:(n_full+1); include n_full predictors, plus response (column 1)
    model0 <- lm(y~.,data=data[,1:(n_full+1)],na.action=na.fail)
    if (method=="dredge") {
        ## multi-model averaging
        require(MuMIn)
        dd <- suppressMessages(dredge(model0))
        if (!is.null(subset)) {
            mm <- model.avg(dd,subset=subset)  ## was: subset=delta<8
        } else  mm <- model.avg(dd)
    } else {
        ## just use the full model
        mm <- model0
    }
    cc <- cbind(coef(mm),confint(mm),
                ## zero intercept
                c(0,attr(data,"true_pars")[1:n_full]))
    dimnames(cc) <- list(param=rownames(cc),
                         val=c("est","lwr","upr","true"))
    return(cc)
}

## coverage
## duplicate of sumfun?
cov_est <- function(arr,method="") {
    covered <- arr[,,"lwr"]<arr[,,"true"] & arr[,,"true"]<arr[,,"upr"]
    cov_est <- colMeans(covered)
    n <- nrow(covered)
    return(data.frame(method=method,par=colnames(covered),cover=cov_est,se=sqrt(cov_est*(1-cov_est)/n)))
}

## tests
if (FALSE) {
    s1 <- simfun(seed=101)
    m1 <- fitfun(s1)
    attr(s1,"true_pars")
    c1 <- cfun0(cc=m1,dd=s1)
}

## 'true' CI
CI_est <- function(arr,method="") {
    resids <- arr[,,"est"]-arr[,,"true"]
    trueCI <- 2*apply(abs(resids),2,quantile,0.95)
    CIwid <- arr[,,"upr"]-arr[,,"lwr"]
    n <- nrow(resids)
    return(data.frame(method=method,par=colnames(resids),true_CIw=trueCI,
                      avg_CIw=colMeans(CIwid),
                      se_CIw=apply(CIwid,2,sd)/sqrt(n)))
}

## just get residuals
res_est <- function(arr,method="") {
    resids <- arr[,,"est"]-arr[,,"true"]
    require(reshape2)
    return(data.frame(method=method,melt(resids)))
}

## get rmse
rmse_est <- function(arr,method="") {
    resids <- arr[,,"est"]-arr[,,"true"]
    return(data.frame(method=method,param=colnames(resids),
                      rmse=sqrt(colSums(resids^2))))
}
