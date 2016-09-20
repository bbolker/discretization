## limit model set (I think this is 'R' in B&A)?
## limit p/n (>10? >20?)
## try AICcmodavg?

library(MASS)
library(plyr)
library(reshape2)
library(ggplot2); theme_set(theme_bw())
library(foreach)
library(doParallel)
registerDoParallel()

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

sumfun <- function(cc,dd=NULL) {
    cc <- cc[rownames(cc)!="(Intercept)",]
    pars <- attr(dd,"true_pars")
    covered <- (cc[,"lwr"] < pars[1:nrow(cc)]) &
        (pars[1:nrow(cc)] < cc[,"upr"])
    return(covered)
}

## tests
if (FALSE) {
    s1 <- simfun(seed=101)
    m1 <- fitfun(s1)
    attr(s1,"true_pars")
    c1 <- cfun0(cc=m1,dd=s1)
}

seedvec <- 101:300
names(seedvec) <- seedvec

fn <-  "c_arr.rds"
if (!file.exists(fn)) {
    ## multiple sims
    ## with plyr
    c_arr <- laply(seedvec,
                   ## function(s) {cat(s,"\n"); fitfun(simfun(seed=s))})
                   function(s) fitfun(simfun(seed=s)),
                   .progress="text")
    dimnames(c_arr)[[1]] <- seedvec
    names(dimnames(c_arr))[1] <- "seed"
    saveRDS(c_arr,fn)
}
c_arr <- readRDS(fn)

## can afford to do longer runs for these, they're fast
seedvec2 <- 101:1000
fn <-  "f_arr.rds"
if (!file.exists(fn)) {
    f_arr <- laply(seedvec2,
                   ## function(s) {cat(s,"\n"); fitfun(simfun(seed=s))})
                   function(s) fitfun(simfun(seed=s),method="full"),
                   .progress="text")
    dimnames(f_arr)[[1]] <- seedvec2
    names(dimnames(f_arr))[1] <- "seed"
    saveRDS(f_arr,fn)
}
f_arr <- readRDS(fn)

fn2 <-  "f20_arr.rds"
if (!file.exists(fn)) {
    f20_arr <- laply(seedvec2,
                   ## function(s) {cat(s,"\n"); fitfun(simfun(seed=s))})
                   function(s) fitfun(simfun(seed=s),method="full",n_full=20),
                   .progress="text")
    dimnames(f20_arr)[[1]] <- seedvec2
    names(dimnames(f20_arr))[1] <- "seed"
    saveRDS(f20_arr,fn)
}
f20_arr <- readRDS(fn)

fn <-  "fzc_arr.rds"
if (!file.exists(fn)) {
    fzc_arr <- laply(seedvec2,
                   ## function(s) {cat(s,"\n"); fitfun(simfun(seed=s))})
                   function(s) fitfun(simfun(seed=s,pcor=0),method="full"),
                   .progress="text")
    dimnames(fzc_arr)[[1]] <- seedvec2
    names(dimnames(fzc_arr))[1] <- "seed"
    saveRDS(fzc_arr,fn)
}
fzc_arr <- readRDS(fn)

resids <- c_arr[,,"est"]-c_arr[,,"true"]
f_resids <- f_arr[,,"est"]-f_arr[,,"true"]
f20_resids <- f20_arr[,,"est"]-f20_arr[,,"true"]
matplot(resids,col=1,cex=0.5,pch=1)
matplot(t(resids),col=1,cex=0.5,pch=1)

## bias
colMeans(resids)
ggplot(melt(resids),aes(x=param,y=value))+
    stat_summary(fun.data=mean_cl_normal)+coord_flip()
## why is the last (smallest true value) variable less biased?

## coverage
## duplicate of sumfun?
cov_est <- function(arr,method="") {
    covered <- arr[,,"lwr"]<arr[,,"true"] & arr[,,"true"]<arr[,,"upr"]
    cov_est <- colMeans(covered)
    n <- nrow(covered)
    return(data.frame(method=method,par=colnames(covered),cover=cov_est,se=sqrt(cov_est*(1-cov_est)/n)))
}
dd <- Map(cov_est,list(c_arr,f_arr,f20_arr,fzc_arr),c("mma","full10","full20","full10_zerocor"))
dd <- do.call(rbind,dd)
ggplot(dd,aes(par,cover,ymin=cover-2*se,ymax=cover+2*se,col=method))+
    geom_pointrange(position=position_dodge(width=0.5))+
    geom_hline(yintercept=0.95,lty=2)+
    scale_colour_brewer(palette="Set1")

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

dd_CI <- Map(CI_est,list(c_arr,f_arr,f20_arr,fzc_arr),c("mma","full10","full20","full10_zerocor"))
dd_CI <- do.call(rbind,dd_CI)
ggplot(dd_CI,aes(x=true_CIw,colour=method))+
    geom_pointrange(aes(y=avg_CIw,ymin=avg_CIw-1.96*se_CIw,
                        ymax=avg_CIw+1.96*se_CIw))+
    geom_abline(slope=1,intercept=0,lty=2)+
    scale_colour_brewer(palette="Set1")

f_CIwid <- colMeans(f_arr[,,"upr"]-f_arr[,,"lwr"])
par(las=1,bty="l")
plot(CIwid,trueCI,ylim=c(0,0.5))
points(f_CIwid,f_trueCI,col=2)
abline(a=0,b=1)

if (FALSE) {
    ## with foreach
    ## DON'T use doParallel (don't want to fry the machine)
    cmat <- foreach(s=seedvec,.combine=rbind) %do% cfun0(seed=s)
}


