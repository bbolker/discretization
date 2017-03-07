seedvec <- 101:300
names(seedvec) <- seedvec

library(plyr)
simfile <- function(x) sprintf("../simdata/%s.rds",x)
batchfun <- function(FUN,seedvec,fn) {
    if (!file.exists(simfile(fn))) {
        arr <- laply(seedvec,
                     FUN,
                     .progress="text")
        dimnames(arr)[[1]] <- seedvec
        names(dimnames(arr))[1] <- "seed"
        saveRDS(arr,simfile(fn))
        return(invisible(arr))
    } else NULL
}
    
source("mmasim_funs.R")

## test
if (FALSE) {
    res <- laply(1:10,
                 function(s) fitfun(simfun(seed=s),method="full"),
                 .progress="text")
}

batchfun(function(s) fitfun(simfun(seed=s)),
         seedvec=101:300,
         fn="c_arr")

## can afford to do longer runs for these, they're fast
batchfun(function(s) fitfun(simfun(seed=s),method="full"),
         seedvec=101:1000,
         fn="f_arr")

batchfun(function(s) fitfun(simfun(seed=s),method="full",n_full=20),
         seedvec=101:1000,
         fn="f20_arr")

batchfun(function(s) fitfun(simfun(seed=s,pcor=0),method="full"),
         seedvec=101:1000,
         fn="fzc_arr")

batchfun(function(s) fitfun(simfun(seed=s,pcor=0,cortype="unif"),
                            method="full"),
         seedvec=101:1000,
         fn="frc_arr")

