#' ---
#' title: "Exploring coverage of multi-model-averaged confidence intervals"
#' author: "Ben Bolker"
#' ---

#+ knitr_setup,message=FALSE, include=FALSE
library(knitr)
opts_chunk$set(echo=FALSE)
## default colour -> Dark2
scale_colour_discrete <- function(..., palette="Dark2"){
	scale_colour_brewer(..., palette=palette)
}

#+ pkgs,message=FALSE
library(MASS)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2); theme_set(theme_bw())
source("R/mmasim_funs.R")

#+ setup
resfiles <- c(c="mma"
            , f="full10"
            , f20="full20"
            , fzc="full10_zerocor"
            , fnc="full10_negcor"
              ##  , frc="full10_unifcor"
              )
results <- setNames(lapply(sprintf("simdata/%s.rds",names(resfiles)),
                           readRDS),
                    resfiles)

##' ## errors
##' 
##' Calculate differences between fitted and true param values; rbind into long form (from list form)

##+ errors/bias

dd_res <- (Map(res_est,results)
    %>% bind_rows(.id="method"))


ggplot(dd_res,aes(x=param,y=value,colour=method, shape=method))+
              stat_summary(fun.data=mean_cl_normal)+
    coord_flip()

##' Some conclusions so far:
##'
##' - biases are due to positive correlation among positive parameters (or negative correlation thereof); could redo with zero correlation, or with uniform correlation, or with randomly flipped signs?
##' - MMA results (done with same models as/comparable to `full10`) are slightly more biased, which is as expected since they're shrinkage estimators
##' - still not sure what's going on with MMA/full comparison for b10? b10 doesn't otherwise look weird
##' - unif cor not working: try some other way to generate random pos-def corr matrices?
##' 
##' ## rmse

#+ rmse
dd_rmse <- (Map(rmse_est,results)
    %>% bind_rows(.id="method")
)    
print(ggplot(dd_rmse,aes(x=param,y=rmse,colour=method, shape=method))
    + geom_point()
    + labs(x="",ylab="root mean square error")
    + coord_flip()
)

##' RMSEs are mostly consistent with bias results - but might expect that
##' MMA would have less variance, so RMSE < full model??
##'
##' ## coverage

#+ coverage
dd <- (Map(cov_est,results)
    %>% bind_rows(.id="method")
)
print(ggplot(dd,aes(par,cover,ymin=cover-2*se,ymax=cover+2*se,
                    col=method,shape=method))
      + geom_pointrange(position=position_dodge(width=0.5))
      + geom_hline(yintercept=0.95,lty=2)
      + coord_flip()
      )

##' coverage results again consistent with bias/RMSE
##' 
##' ## confidence interval widths

##' This is an attempt to compute what the CI widths should
##' be in order to achieve nominal coverage, and compare them
##' to actual CI widths ...
##' 
##+ CI_wid

dd_CI <- (Map(CI_est,results)
    %>% bind_rows(.id="method")
)
print(ggplot(dd_CI,aes(x=true_CIw,colour=method, shape=method))
      +  geom_pointrange(aes(y=avg_CIw,ymin=avg_CIw-1.96*se_CIw,
                             ymax=avg_CIw+1.96*se_CIw))
      + geom_abline(slope=1,intercept=0,lty=2)
      + scale_colour_brewer(palette="Set1")
      )


