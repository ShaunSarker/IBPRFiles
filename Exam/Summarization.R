ess = read.csv("terrorism-ess.csv", header = TRUE)
germany = ess[ess$cname == "Germany",]
g_im = germany$imm7
g_eu = germany$eu5
attack = germany$post_attack
median(g_eu)
median(g_im)
median(germany$eu5)

boxplot(g_im ~ attack, data = germany, xlab = "Post_Attack", ylab = "eu score", main = "immigration data")

#gerPreAtk = germany[germany$post_attack== 0,]
#gerPostAtk = germany[germany$post_attack == 1,]