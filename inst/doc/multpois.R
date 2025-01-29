## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(car)
library(nnet)
library(lme4)
library(lmerTest)
library(emmeans)

## ----setup--------------------------------------------------------------------
library(multpois)

## -----------------------------------------------------------------------------
data(bs2, package="multpois")
bs2$PId = factor(bs2$PId)
bs2$Y = factor(bs2$Y, levels=c("yes","no"))
bs2$X1 = factor(bs2$X1)
bs2$X2 = factor(bs2$X2)
contrasts(bs2$X1) <- "contr.sum"
contrasts(bs2$X2) <- "contr.sum"

## ----fig.cap="**Figure 1.** Proportions of `yes` (green) and `no` (pink) responses in four conditions: `{a, c}`, `{a, d}`, `{b, c}`, and `{b, d}`.", fig.height=4.5, fig.width=4----
xt = xtabs( ~ X1 + X2 + Y, data=bs2)
mosaicplot(xt, main="Y by X1, X2", las=1, col=c("lightgreen","pink"))

## ----message=FALSE, warning=FALSE---------------------------------------------
m1 = glm(Y ~ X1*X2, data=bs2, family=binomial)
Anova(m1, type=3)
emmeans(m1, pairwise ~ X1*X2, adjust="holm")$contrasts

## ----message=FALSE, warning=FALSE---------------------------------------------
m2 = glm.mp(Y ~ X1*X2, data=bs2)
Anova.mp(m2, type=3)
glm.mp.con(m2, pairwise ~ X1*X2, adjust="holm")

## -----------------------------------------------------------------------------
data(bs3, package="multpois")
bs3$PId = factor(bs3$PId)
bs3$Y = factor(bs3$Y, levels=c("yes","no","maybe"))
bs3$X1 = factor(bs3$X1)
bs3$X2 = factor(bs3$X2)
contrasts(bs3$X1) <- "contr.sum"
contrasts(bs3$X2) <- "contr.sum"

## ----fig.cap="**Figure 2.** Proportions of `yes` (green), `no` (pink), and `maybe` (yellow) responses in four conditions: `{a, c}`, `{a, d}`, `{b, c}`, and `{b, d}`.", fig.height=4.5, fig.width=4----
xt = xtabs( ~ X1 + X2 + Y, data=bs3)
mosaicplot(xt, main="Y by X1, X2", las=1, col=c("lightgreen","pink","lightyellow"))

## ----message=FALSE, warning=FALSE---------------------------------------------
m3 = multinom(Y ~ X1*X2, data=bs3, trace=FALSE)
Anova(m3, type=3)

## ----message=FALSE, warning=FALSE---------------------------------------------
e0 = emmeans(m3, ~ X1*X2 | Y, mode="latent")
c0 = contrast(e0, method="pairwise", ref=1)
test(c0, joint=TRUE, by="contrast")

## ----message=FALSE, warning=FALSE---------------------------------------------
m4 = glm.mp(Y ~ X1*X2, data=bs3)
Anova.mp(m4, type=3)
glm.mp.con(m4, pairwise ~ X1*X2, adjust="holm")

## -----------------------------------------------------------------------------
data(ws2, package="multpois")
ws2$PId = factor(ws2$PId)
ws2$Y = factor(ws2$Y, levels=c("yes","no"))
ws2$X1 = factor(ws2$X1)
ws2$X2 = factor(ws2$X2)
contrasts(ws2$X1) <- "contr.sum"
contrasts(ws2$X2) <- "contr.sum"

## ----fig.cap="**Figure 3.** Proportions of `yes` (green) and `no` (pink) responses in four conditions: `{a, c}`, `{a, d}`, `{b, c}`, and `{b, d}`.", fig.height=4.5, fig.width=4----
xt = xtabs( ~ X1 + X2 + Y, data=ws2)
mosaicplot(xt, main="Y by X1, X2", las=1, col=c("lightgreen","pink"))

## ----message=FALSE, warning=FALSE---------------------------------------------
m5 = glmer(Y ~ X1*X2 + (1|PId), data=ws2, family=binomial)
Anova(m5, type=3)
emmeans(m5, pairwise ~ X1*X2, adjust="holm")$contrasts

## ----message=FALSE, warning=FALSE---------------------------------------------
m6 = glmer.mp(Y ~ X1*X2 + (1|PId), data=ws2)
Anova.mp(m6, type=3)
glmer.mp.con(m6, pairwise ~ X1*X2, adjust="holm")

## -----------------------------------------------------------------------------
data(ws3, package="multpois")
ws3$PId = factor(ws3$PId)
ws3$Y = factor(ws3$Y, levels=c("yes","no","maybe"))
ws3$X1 = factor(ws3$X1)
ws3$X2 = factor(ws3$X2)
contrasts(ws3$X1) <- "contr.sum"
contrasts(ws3$X2) <- "contr.sum"

## ----fig.cap="**Figure 4.** Proportions of `yes` (green), `no` (pink), and `maybe` (yellow) responses in four conditions: `{a, c}`, `{a, d}`, `{b, c}`, and `{b, d}`.", fig.height=4.5, fig.width=4----
xt = xtabs( ~ X1 + X2 + Y, data=ws3)
mosaicplot(xt, main="Y by X1, X2", las=1, col=c("lightgreen","pink","lightyellow"))

## ----message=FALSE, warning=FALSE---------------------------------------------
m7 = glmer.mp(Y ~ X1*X2 + (1|PId), data=ws3)
Anova.mp(m7, type=3)
glmer.mp.con(m7, pairwise ~ X1*X2, adjust="holm")

## -----------------------------------------------------------------------------
data(icecream, package="multpois")
icecream$PId = factor(icecream$PId)
icecream$Pref = factor(icecream$Pref, levels=c("vanilla","chocolate","strawberry"))
icecream$Age = factor(icecream$Age, levels=c("adult","child"))
icecream$Season = factor(icecream$Season, levels=c("fall","winter","spring","summer"))
contrasts(icecream$Age) <- "contr.sum"
contrasts(icecream$Season) <- "contr.sum"

## ----fig.cap="**Figure 5.** Proportions of `vanilla` (beige), `chocolate` (brown), and `strawberry` (pink) responses for adults and children across the four seasons.", fig.height=6, fig.width=7----
xt = xtabs( ~ Age + Season + Pref, data=icecream)
mosaicplot(xt, main="Pref by Age, Season", las=1, col=c("beige","tan","pink"))

## ----message=FALSE, warning=FALSE---------------------------------------------
m8 = glmer.mp(Pref ~ Age*Season + (1|PId), data=icecream)
Anova.mp(m8, type=3)

## ----fig.cap="**Figure 6.** Proportions of `vanilla` (beige), `chocolate` (brown), and `strawberry` (pink) responses for adults and children. The main effect of `Age` emerges, with children preferring chocolate more and strawberry less than adults.", fig.height=6, fig.width=7----
xt = xtabs( ~ Age + Pref, data=icecream)
mosaicplot(xt, main="Pref by Age", las=1, col=c("beige","tan","pink"))

## ----fig.cap="**Figure 7.** Proportions of `vanilla` (beige), `chocolate` (brown), and `strawberry` (pink) responses by `Season`. Although there are some differences in proportion, they are not quite statistically significant (*p* = 0.052).", fig.height=6, fig.width=7----
xt = xtabs( ~ Season + Pref, data=icecream)
mosaicplot(xt, main="Pref by Season", las=1, col=c("beige","tan","pink"))

## ----message=FALSE, warning=FALSE---------------------------------------------
glmer.mp.con(m8, pairwise ~ Age*Season, adjust="holm")

## ----message=FALSE, warning=FALSE---------------------------------------------
glmer.mp.con(m8, pairwise ~ Age*Season, adjust="none")

## -----------------------------------------------------------------------------
p.adjust(c(0.017176, 0.308026, 0.001020, 0.363038), method="holm")

