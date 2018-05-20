args = commandArgs(trailingOnly=TRUE)
print(args)

## TESTING
## args <- c("test", "20", "10", "mma", "10", "0.3", "compsym")
simfile <- sprintf("simdata/%s.rds",args[1])
n_full <- as.numeric(args[2])
n_rep <- as.numeric(args[3])
method <- args[4]
n_full <- as.numeric(args[5])
pcor <- as.numeric(args[6])
ctype <- args[7]

library(plyr)
library(dplyr)

batchfun <- function(FUN,seedvec) {
    arr <- laply(seedvec, FUN, .progress="text")
    dimnames(arr)[[1]] <- seedvec
    names(dimnames(arr))[1] <- "seed"
    return(arr)
}

source("R/mmasim_funs.R")

## test
if (FALSE) {
    res <- laply(1:10,
                 function(s) fitfun(simfun(seed=s),method="full"),
                 .progress="text")
}

fs <- function(s) {
    simfun(s, pcor=pcor, cortype=ctype) %>%
        fitfun(method=method, n_full=n_full)
}

arr <- batchfun(fs, seedvec=seq(n_rep))

saveRDS(arr, file=simfile)



