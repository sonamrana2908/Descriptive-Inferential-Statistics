library(SDSFoundations)
res <- TempskiResilience
View(res)

#Pre-Lab
res[res$MS.QoL == 10,"Age"]
res$MS.QoL[1:10]

Clgrp <- res[res$Group =='Clinical Sciences',]

var <- c('QoL','BDI')
cor(Clgrp[,var])

ov_mod <- lm(QoL~BDI,data=Clgrp)
summary(ov_mod)
confint(ov_mod)

plot(ov_mod,which=1)
cutoff <- 4/(ov_mod$df)
plot(ov_mod,which=4,cook.levels = cutoff,id.n=6)

names(Clgrp)
var2 <- c('DREEM.S.SP','DREEM.A.SP','Resilience','BDI','Age','MS.QoL')
cor(Clgrp[,var2],use="pairwise.complete.obs")

library(psych)
corr.test(Clgrp[,var2],use="pairwise.complete.obs")

ms_mod <- lm(MS.QoL~DREEM.S.SP+DREEM.A.SP+Resilience+BDI+Age,data=Clgrp)
summary(ms_mod)
confint(ms_mod)

library(car)
vif(ms_mod)
1/vif(ms_mod)
plot(ms_mod,which=1)
cutoff2 <- 4/(ms_mod$df)
plot(ms_mod,which=4,cook.levels=cutoff2)

lmBeta(ms_mod)
pCorr(ms_mod)
round(pCorr(ms_mod),4)

#Lab

bs <- res[res$Group =='Basic Sciences',]
names(res)
var3 <- c('MS.QoL','WHOQOL.PH','WHOQOL.PSY','WHOQOL.SOC','WHOQOL.ENV')
cor(bs[,var3])

bs_mod <- lm(MS.QoL~WHOQOL.PH+WHOQOL.PSY+WHOQOL.SOC+WHOQOL.ENV,data=bs)
summary(bs_mod)
plot(bs_mod,which=1)
lmBeta(bs_mod)
pCorr(bs_mod)

#Question Set 1-3
names(res)

var4 <- c('BDI','State.Anxiety','Trait.anxiety','Female','Age')
cor(Clgrp[,var4])

bdi_mod <- lm(BDI~State.Anxiety+Trait.anxiety+Female+Age,data=Clgrp)
summary(bdi_mod)

lmBeta(bdi_mod)
pCorr(bdi_mod)

1848.76/69.22
26.71*18
-23.4325/12.74
8.32*0.1528
-23.4325+1.2713
3526.4+12*90.12+12.69+15*23.406+722.5
