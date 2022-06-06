library(glmnet)
library(leaps)
library(ISLR)
library(broom)
library(tidyverse)
library(furrr)
library(progressr)
library(bcaboot)

## FIXME: figure out how seeds really work
fopts <- furrr_options(seed = 101)
plan(multicore, workers = 20)


theme_set(theme_bw())

self_name <- function(x) {
  names(x) <- x
  return(x)
}

simfun <- function(n, p, rng = 1, sig_x = 1, sig_r = 1 ) {
  beta <- runif(p, min = -rng, max = rng)
  X <- matrix(rnorm(n*p, sd = sig_x), ncol = p)
  dd <- data.frame(y = rnorm(n, X %*% beta, sd = sig_r),
                   X)
  list(beta = setNames(beta, names(dd)[-1]), dd = dd)
}

fitfun <- function(dd, alpha = 0) {
  glmnet(y = dd[["y"]], x = as.matrix(dd[!names(dd) == "y"]), alpha = alpha,
         intercept = FALSE)
}

my_tidy <- function(L, c_level = 0.9, beta) {
  res <- (L %>%
          purrr::map_dfr(tidy,
                         conf.int = TRUE,
                         conf.level = c_level,
                         .id = "model")
    |> select(model, term, estimate, lwr = conf.low, upr = conf.high)
    |> full_join(tibble(term = names(beta), true = beta), by = "term")
  )
  return(res)
}

order_terms <- . %>% mutate(across(term, ~reorder(factor(.), estimate)))

wrap_binom.test <- function(x, n) {
    if (is.na(x)) {
        warning("NA x")
        return(list(conf.int = rep(NA_real_, 2)))
    }
    if (x < 0 || trunc(x) != x) {
        warning("neg or integer x: ", x)
        return(list(conf.int = rep(NA_real_, 2)))
    }
    return(binom.test(x = x, n = n))
}

get_coverage <- (.
  %>% na.omit()
  %>% summarise(n = n(),
                n_ok = sum(lwr < true & true < upr, na.rm = TRUE))
  %>% rowwise()
  %>% mutate(
          prop = n_ok/n,
          lwr = wrap_binom.test(x = n_ok, n = n)$conf.int[1],
          upr =  wrap_binom.test(x = n_ok, n = n)$conf.int[2])
)


## https://cran.r-project.org/web/packages/bcaboot/vignettes/bcaboot.html
glmnet_boot <- function(B, X, y, glmnet_model, weights = rep(1, length(y)),
                        var = "resp",
                        family = "gaussian", lchoice = "lambda.min",
                        intercept = FALSE) {
  n <- length(y)
  if (inherits(glmnet_model, "cv.glmnet")) {
    ## full-CV
    lambda <- switch(lchoice,
                     lambda.1se = glmnet_model[[lchoice]],
                     lambda.minval = min(glmnet_model$lambda),
                     lambda.maxval = max(glmnet_model$lambda))
    lambda <- glmnet_model[[lchoice]]
    theta <- as.matrix(coef(glmnet_model, s = lambda))
    yhat <- predict(glmnet_model, newx = X, s = lchoice, type = "response")
  } else {
    ## specified grid
    lambda <- switch(lchoice,
                     lambda.min = with(glmnet_model, lambda[which.min(dev.ratio)]),
                     lambda.minval = min(glmnet_model$lambda)
                     )
    theta <- as.matrix(coef(glmnet_model))
    yhat <- predict(glmnet_model, newx = X, type = "response")
  }
  if (family == "binomial") {
    y_star <- sapply(seq_len(B), function(i) ifelse(runif(n) <= yhat, 1, 0))
  } else {
    ## Gaussian family parametric bootstrap ...
    nz <- sum(predict(glmnet_model, newx = X, s = lchoice, type ="coef") != 0)
    sigma <- sqrt(sum((y - yhat)^2)/(n - nz - 1))
    y_star <- sapply(seq_len(B), function(i) rnorm(n, mean = yhat, sd = sigma))
  }
  beta_star <- apply(y_star, 2,
                     function(y) {
                       as.matrix(coef(glmnet::glmnet(x = X, y = y, lambda = lambda, weights = weights, family = family,
                                                     intercept = intercept)))
                     })
  rownames(beta_star) <- rownames(theta)
  list(theta = theta[var, ],
       theta_star = beta_star[var, ],
       suff_stat = t(y_star) %*% X)
}

extract_levels <- function(x, ci_level = 0.9) {
  clevs  <- c((1-ci_level)/2, (1+ci_level)/2)
  (x
    %>% filter(abs(level-clevs[1])<1e-8 | abs(level-clevs[2])<1e-8)  ## ugh
    %>% mutate(across(level, factor, levels = clevs, labels = c("lwr", "upr")))
    %>% pivot_wider (names_from = level, values_from = value)
  )
}

#### end utils

## example 1: full vs stepwise regression
set.seed(101)
ss <- simfun(n = 100, p = 90, sig_r = 5)

## fit without intercept (no real loss of generality, intercept was 0 in initial sim
## just have to be careful to be consistent ...
fullfit  <- lm (y ~ . -1 , data = ss$dd)
fit_step <- step(fullfit, trace=0)

tt <- (list(full=fullfit, step = fit_step)
  |> my_tidy(beta = ss$beta)
  |> order_terms()
)

## not sure why estimates are so terrible, guess it's just overfitting thing?
plot(estimate ~ true, data = tt, col = factor(tt$model), pch=16)
abline(lm(estimate ~ true, data = tt))
abline(a = 0, b= 1, col="blue")

gg1 <- (ggplot(tt, aes(estimate, term,  colour = model))
  + geom_linerange(aes(size = model, xmin = lwr, xmax = upr))
  ## + geom_point()
  + scale_colour_manual(values = c("red", "black"))
  + scale_size_manual(values = c(1,2))
  + theme(axis.text.y = element_blank())
  ## + geom_point(aes(x = true), colour = "blue")
)
ggsave(gg1, file = "stepcoef.png", width = 7, height = 6)

simfun2 <- function(r, c_level = 0.9, seed = NULL) {
  if (!is.null(seed)) set.seed()
  cat(".")
  ss <- simfun(n = 100, p = 90, sig_r = 5)
  fullfit  <- lm (y ~ ., data = ss$dd)
  fit_step <- step(fullfit, trace=0)
  tt <- (list(full=fullfit, step = fit_step)
    |> my_tidy(beta = ss$beta, c_level = c_level)
    |> order_terms()
    |> mutate(rep = r)
  )
  return(tt)
}

## coverage calculation
set.seed(101)
allres <- furrr::future_map_dfr(1:200, simfun2)
allres_step_tbl <- (allres
  %>% group_by(model)
  %>% get_coverage()
)

obj_list  <- "allres_step_tbl"
save(list = obj_list, file = "ares_sims.rda")


## now do the same for glmnet (with parameters via bcaboot: check with lambda -> 0?)

y <- ss$dd[["y"]]
X <- as.matrix(ss$dd[!names(ss$dd) == "y"])
fit_ridge0 <- glmnet(y = y, x = X, alpha = 0, intercept = FALSE, lambda = 1e-10)
fit_ridgecv <- cv.glmnet(y = y, x = X, alpha = 0, intercept = FALSE)
plot(fit_ridgecv)

## run parametric bootstrap, BCA code from bcaboot, extract limits
get_bca_ci <- function(B, y, X, fitted_model, var = "X1") {
  g <- glmnet_boot(B = B, y = y, X = X, fitted_model, var = var)
  g_bca <- with(g, bcapar(t0 = theta,
                          tt = theta_star,
                          bb = suff_stat))
  tibble(level = as.numeric(rownames(g_bca$lims)),
         value = g_bca$lims[,"bca"])
}


set.seed(101)
get_bca_ci(B = 2000, y = y, X = X, fitted_model = fit_ridge0, var = "X1")
xvars <- self_name(paste0("X", 1:90))

with_progress(fci_0 <- (xvars
  %>% furrr::future_map_dfr(.f = get_bca_ci, B = 2000, y = y, X = X, fitted_model = fit_ridge0, .id = "term",
                            .options = fopts)
))

fci_0_b <- extract_levels(fci_0)

fci_cv <- (xvars
  %>% furrr::future_map_dfr(.f = get_bca_ci, B = 2000, y = y, X = X, fitted_model = fit_ridgecv, .id = "term",
                            .options = fopts))


fci_cv_b <- extract_levels(fci_cv)

simfun3 <- function(r, c_level = 0.9, seed = NULL) {
  if (!is.null(seed)) set.seed()
  cat(".")
  ss <- simfun(n = 100, p = 90, sig_r = 5)
  fullfit  <- lm (y ~ ., data = ss$dd)
  xvars <- self_name(paste0("X", 1:90))
  fci_cv <- furrr::map_dfr(.f = get_bca_ci, B = 2000, y = y, X = X, fitted_model = fit_ridgecv, .id = "term",
                           .options = fopts)
  fit_ridgecv <- cv.glmnet(y = y, x = X, alpha = 0, intercept = FALSE)

  tt <- (list(full=fullfit, step = fit_step)
    |> my_tidy(beta = ss$beta, c_level = c_level)
    |> order_terms()
    |> mutate(rep = r)
  )
  return(tt)
}

save.image(file = "ares_sims_bak.rda")
tt_ridge0 <- (tidy(fit_ridge0)
  %>% filter(lambda == min(fit_ridge0$lambda))
  %>% select(term, estimate)
  %>% full_join(fci_0_b, by = "term")
)

tt_ridgecv <- (predict(fit_ridgecv, type = 'coef', newx = X, lambda = "lambda.min")
  %>% as.matrix()
  %>% as.data.frame()
  %>% rownames_to_column("term")
  %>% rename(estimate = "lambda.1se")
  %>% filter(term != "(Intercept)")
  %>% full_join(fci_cv_b, by = "term")
)

tt_full <- tidy(fullfit, conf.int = TRUE, conf.level = 0.9) %>% select(term, estimate, lwr = conf.low, upr = conf.high)
tt_comb <- bind_rows(list(ridge0 = tt_ridge0, full = tt_full), .id = "model") |> order_terms()

tt_comb_cv <- bind_rows(list(ridgecv = tt_ridgecv, full = tt_full), .id = "model") |> order_terms()


gg2A <- (ggplot(order_terms(tt_full),
                aes(estimate, term))
  + geom_linerange(aes(xmin = lwr, xmax = upr))
  + theme(axis.text.y = element_blank())
)
cowplot::plot_grid(gg2A + ggtitle("full model"),
                   gg2A %+% order_terms(tt_ridgecv) + ggtitle("ridge"))

## work on this in the morning!

gg1 %+% tt_comb

gg1 %+% tt_comb_cv

fci_cv <- (xvars
  %>% furrr::future_map_dfr(.f = get_bca_ci, B = 2000, y = y, X = X, fitted_model = fit_ridgecv, .id = "var",
                            .options = fopts)
)

ridge0c <- drop(coef(fit_ridge0))[-1]
plot(ridge0c, coef(fullfit))
## all.equal( coef(fullfit))


ff <- fitfun(ss$dd)
ff$beta[,100]
par2 <- coef(lm(y ~ . -1, ss$dd))
image(ff$beta)

## subtract true beta
beta_dev <- sweep(ff$beta, MARGIN = 1, STATS = ss$beta, FUN = "-")
plot(sqrt(colSums(beta_dev^2)))

beta_dev2 <- sweep(ff$beta, MARGIN = 1, STATS = par2, FUN = "-")
## plot.cv.glmnet(ff)
plot(sqrt(colSums(beta_dev2^2)))

plot(ff$beta[1,] - ss$beta[1])
plot(ff$beta[1,])

plot(ff)

combfun <- function(n = 200, p = 12, ..., alpha = 1) {
  ss <- simfun(n = n, p = p, ...)
  ff <- fitfun(ss$dd)
  as.matrix(sweep(ff$beta, MARGIN = 1, STATS = ss$beta, FUN = "-"))
}

rr <- replicate(150, combfun(sig_r = 5))
names(dimnames(rr)) <- c("par", "lambda", "rep")
## bias in parameter estimation


## (how) does bias/variance in parameter estimates
## what is the analog of training/test MSE ? (especially training MSE?)

## how do we construct the empirical CI
## https://stackoverflow.com/questions/39750965/confidence-intervals-for-ridge-regression
##  which is typically not the case in situations in which penalized estimates are used.
## can get bias^2




g1 <- glmnet_boot(2000, y = ss$dd[["y"]], X = as.matrix(ss$dd[,-1]), cc, var = "X1")
g1 <- glmnet_boot(2000, y = ss$dd[["y"]], X = as.matrix(ss$dd[,-1]), cc, var = "X3")
glmnet_bca <- with(g1, bcapar(t0 = theta,
                              tt = theta_star,
                              bb = suff_stat))


Xy <- as.matrix(ss$dd)
rfun <- function(Xy, i = 1) {
  y <- Xy[, 1]
  X <- Xy[, -1]
  cc <- cv.glmnet(y = y, x = X, alpha = 0, intercept = FALSE)
  lambda <- cc$lambda.min
  cc$glmnet.fit$beta[,cc$lambda == cc$lambda.min][i]
}
set.seed(1234)
## n = 442 = 34 * 13
bcajack(x = Xy, B = 1000, func = rfun, m = 34, verbose = TRUE)

bcajack(x = Xy, B = 1000, func = rfun, m = 34, verbose = TRUE)



jackfun <- function(n, p, ..., boot_ind = 1, B = 1000, boot_alpha = c(0.025, 0.05, 0.1)) {
  rfun <- function(Xy, i = boot_ind) {
    y <- Xy[, 1]
    X <- Xy[, -1]
    cc <- cv.glmnet(y = y, x = X, alpha = 0, intercept = FALSE)
    lambda <- cc$lambda.min
    cc$glmnet.fit$beta[,cc$lambda == cc$lambda.min][i]
  }
  ss <- simfun(n, p, ...)
  b <- bcajack(as.matrix(ss$dd), B = B, func = rfun, m = 34, alpha = boot_alpha, verbose = TRUE)
  c(ss$beta[boot_ind], b$lims[, "bca"])
}

parfun <- function(n, p, ..., boot_ind = 1, B = 1000, boot_alpha = c(0.025, 0.05, 0.1)) {
  ss <- simfun(n, p, ...)
  y <- ss$dd[["y"]]
  X <- as.matrix(ss$dd[,-1]) ## assume y is first
  cc <- cv.glmnet(y = y, x = X, alpha = 0, intercept = FALSE)
  g1 <- glmnet_boot(B, y = y, X = X, cc, var = names(ss$dd)[boot_ind+1])
  b <- with(g1, bcapar(t0 = theta,
                       tt = theta_star,
                       bb = suff_stat))
  c(ss$beta[boot_ind], b$lims[, "bca"])
}

res <- replicate(100, jackfun(100, 90, sig_r = 5, B = 1000))
library(parallel)
cl <- makeCluster(20)
clusterEvalQ(cl,library(bcaboot))
clusterEvalQ(cl,library(glmnet))
clusterExport(cl,c("jackfun", "simfun", "parfun", "glmnet_boot"))
clusterSetRNGStream(cl)
## ss <- parSapply(cl, 1:200, function(i,...) jackfun(100, 90, sig_r = 5, B = 1000))
sspar <- parSapply(cl, 1:200, function(i,...) parfun(100, 90, sig_r = 5, B = 2000))
#stop the cluster
stopCluster(cl)

tss <- t(ss)
alpha_ncov_fun <- function(alpha, tss) {
  lims <- tss[,as.character(c(alpha/2, (1-alpha/2)))]
  sum(tss[,1] > lims[,1] & tss[,1] < lims[,2])
}
alpha_ncov <- sapply(c(0.05,0.1,0.2), alpha_ncov_fun)
alpha_ncov_par <- sapply(c(0.05,0.1,0.2), alpha_ncov_fun, tss = t(sspar))
bfun <- function(s, n = 200) {
  bb <- binom.test(s, n)
  with(bb, setNames(c(estimate, conf.int), c("est", "lwr", "upr")))
}
cbind(c(0.95, 0.9, 0.8), t(sapply(alpha_ncov, bfun)))
## bcajack doesn't do bias correction??? or doesn't do it well ...

## what if we do this for known lambda?


## what would I need to construct CVs?
## assume translation
## what are the 95% quantiles of errors in beta?

###
library(leaps)
regfit.full <- regsubsets(Salary~., data = hh, nvmax = 19)
summary(regfit.full)$cp
plot(regfit.full, scale = "Cp")

## replicate(
##     cc <- cv.glmnet(y = y, x = X, alpha = 0, intercept = FALSE)
##   lambda <- glmnet_model[[lchoice]]
##   theta <- as.matrix(coef(glmnet_model, s = lambda))


