pkgs <- c("ISLR", "MASS", "bcaboot", "broom", "directlabels", "dplyr", "furrr", "ggplot2", "glmnet", "hdm", "knitr", "leaps", "parallel", "plyr", "progressr", "reshape2", "rvest", "tidyverse")
ip <- installed.packages()
to_install <- setdiff(pkgs, rownames(ip))
install.packages(to_install)
