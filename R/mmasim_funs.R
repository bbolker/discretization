## limit model set (I think this is 'R' in B&A)?
## limit p/n (>10? >20?)
## try AICcmodavg?

library(MASS)

##' @param n_true number of parameters
##' @param g geometric decay parameter
mkpars <- function(n_true=20,g=0.8) {
    return(g^(0:(n_true-1)))
}
    
##' @param n_true dimensionality of true model
##' @param g geometric decrease in effect size
##' @param pcor correlation among parameters
##' @param cortype type of correlation
##' \itemize{
##' \item "compsym": compound symmetric with correlation \code{pcor}
##' \item "unif": uniform with range (-pcor,pcor)
##' \item "zero": zero correlation
##' }
##' @param stddev standard deviation of response
##' @param seed random-number seed
##' @param N sample size
simfun <- function(seed=NULL,
                   N=1000,
                   n_true=20,
                   g=0.8,
                   pars=NULL,
                   pcor=0.3,
                   cortype=c("compsym","unif","zero"),
                   stddev=2) {
    maxtries <- 1000
    require(MASS)
    if (!is.null(seed)) set.seed(seed)

    if (is.null(pars)) pars <- mkpars(n_true,g)
    if (pcor==0 || cortype=="zero") {
        m <- matrix(rnorm(N*n_true),N,n_true)
    } else {
        if (cortype=="compsym") {
            S <- matrix(pcor,n_true,n_true)
            diag(S) <- 1
        } else if (cortype=="unif") {  ## random cors
            S <- matrix(1,n_true,n_true)
            tries <- 0
            while(tries<maxtries) {
                ## FIXME: can't work in maxtries
                ## try some other method of generating random corr matrices?
                S[upper.tri(S)] <- runif(n_true*(n_true-1)/2,
                                         -abs(pcor),abs(pcor))
                S <- matrix(Matrix::forceSymmetric(S),n_true,n_true)
                mineig <- min(eigen(S,only.values=TRUE)$values)
                cat(mineig,"\n")
                if (mineig>0) break
                tries <- tries+1
            }
            if (tries==maxtries) stop("maxtries reached in unif cortype calc")
        }
        m <- MASS::mvrnorm(N,mu=rep(0,n_true),Sigma=S)
    }
    if (n_true>99) warning("n_true>99; parameters may not be properly sorted")
    dimnames(m) <- list(NULL,sprintf("b%0.2d",1:n_true))
    beta <- pars  ## no intercept
    mu <- m %*% beta
    y <- rnorm(N,mu,sd=stddev)
    dd <- data.frame(y,m)
    attr(dd,"true_pars") <- pars
    return(dd)
}

##' @param data
##' @param n_full Number of parameters for maximal model
	## JD thinks the default should be NULL => all 
##' @param method dredge for multi-model averaging, full for just using the full (maximal) model
##' @param subset Choose a subset of dredged models to average
fitfun <- function(data, n_full=10, 
                   method=c("mma","full"),
						 subset=NULL)
{
    method <- match.arg(method)
    ## 1:(n_full+1); include n_full predictors, plus response (column 1)
    dd <- data[,1:(n_full+1)]
    ## zero-intercept model
    model0 <- lm(y~.-1,data=dd,na.action=na.fail)
    if (method=="mma") {
        ## multi-model averaging
        require(MuMIn)
        dd <- suppressMessages(dredge(model0))
        if (!is.null(subset)) {
            mm <- model.avg(dd,subset=subset)  ## was: subset=delta<8
        } else  mm <- model.avg(dd)
    } else if (method=="full") {
        ## just use the full model
        mm <- model0
    }
    cc <- cbind(coef(mm),confint(mm),attr(data,"true_pars")[1:n_full])
    dimnames(cc) <- list(param=rownames(cc),
                         val=c("est","lwr","upr","true"))
    return(cc)
}

## coverage
## duplicate of sumfun?
cov_est <- function(arr) {
    covered <- arr[,,"lwr"]<arr[,,"true"] & arr[,,"true"]<arr[,,"upr"]
    cov_est <- colMeans(covered)
    n <- nrow(covered)
    return(data.frame(par=colnames(covered),
                      cover=cov_est,
                      se=sqrt(cov_est*(1-cov_est)/n),
                      stringsAsFactors=FALSE))
}

## tests
if (FALSE) {
    s1 <- simfun(seed=101)
    m1 <- fitfun(s1)
    attr(s1,"true_pars")
    c1 <- cfun0(cc=m1,dd=s1)
}

## 'true' CI
CI_est <- function(arr) {
    resids <- arr[,,"est"]-arr[,,"true"]
    trueCI <- 2*apply(abs(resids),2,quantile,0.95)
    CIwid <- arr[,,"upr"]-arr[,,"lwr"]
    n <- nrow(resids)
    return(data.frame(par=colnames(resids),
                      true_CIw=trueCI,
                      avg_CIw=colMeans(CIwid),
                      se_CIw=apply(CIwid,2,sd)/sqrt(n),
                      stringsAsFactors=FALSE))
}

## Get difference between estimate and true parameters and melt into long form
res_est <- function(arr) {
    resids <- arr[,,"est"]-arr[,,"true"]
    return(reshape2::melt(resids,as.is=TRUE))
}

## get rmse
rmse_est <- function(arr) {
    resids <- arr[,,"est"]-arr[,,"true"]
    return(data.frame(param=colnames(resids),
                      rmse=sqrt(colSums(resids^2)),
                      stringsAsFactors=FALSE))
}
