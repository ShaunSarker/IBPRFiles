#2.5 and 2.6 and 2.7
install.packages("tidyverse")
install.packages("texreg")
install.packages("stargazer")
install.packages("margins")
library(margins)
library(texreg)
library(tidyverse)
library(stargazer)
ess = read.csv("terrorism-ess.csv", header = TRUE)
israel = ess[ess$cname == "Israel",]
israelPreAtk = israel[israel$post_attack == "0",]
israelPostAtk = israel[israel$post_attack == "1",]
germany = ess[ess$cname == "Germany",]
germanyPreAtk = germany[germany$post_attack == "0",]
germanyPostAtk = germany[germany$post_attack == "1",]
g_im = germany$imm7
g1 = germanyPreAtk$imm7
g2 = germanyPostAtk$imm7
i_im = israel$imm7
i1 = israelPreAtk$imm7
i2 = israelPostAtk$imm7

iModel = lm(israel$imm7 ~ israel$post_attack)

coef(iModel)

cor.test(israel$post_attack, israel$imm7)

summary(smallModel)

gModel = lm(germany$imm7 ~ germany$post_attack, data = germany)

coef(gModel)

cor.test(germany$imm7, germany$post_attack)

extReg = lm(israel$imm7 ~ israel$post_attack + israel$male + israel$edu + israel$age + israel$income + israel$unemployed + israel$lrscale)

cor.test(israel$imm7, israel$lrscale)

screenreg(list(extReg), file = NULL, stars = c(0.01, 0.05, 0.1), digits = 4,  anova(extReg))


gcHet = lm(imm7 ~ post_attack * lrscale, data = germany)
summary(gcHet)

gcHet.t = lm(imm7 ~ post_attack:lrscale, data = germany)
summary(gcHet.t)
coef(gcHet)
screenreg(list(gcHet), file = NULL, stars = c(0.01, 0.05, 0.1), digits = 3,  anova(gcHet))



cplot(gcHet,
     main = "Political Ideology affects",
     xlab = "Before/After the Attack",
     ylab = "Immigration Scale Score",
)

