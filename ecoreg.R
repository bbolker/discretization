dd <- readRDS("~/projects/mmd_utils/ecoreg.rds")
plot(Feat_log_sc ~ NPP_log_sc, dd)
cor.test(~ NPP_log_sc + Feat_log_sc, dd)

m1 <- lm(mbirds_log ~ Feat_log_sc + NPP_log_sc,
        data = dd)
cov2cor(vcov(m))
car::vif(m)

plot(Feat_log_sc ~ NPP_log_sc, dd)
m2 <- lm(mbirds_log ~ Feat_log_sc + NPP_log_sc +
             NPP_cv_sc + Feat_cv_sc,
        data = dd)
cov2cor(vcov(m2))
car::vif(m2)
coef(summary(m2))
