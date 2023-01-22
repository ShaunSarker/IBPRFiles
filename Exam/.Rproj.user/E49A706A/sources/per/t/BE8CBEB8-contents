ess = read.csv("terrorism-ess.csv", header = TRUE)
germany = ess[ess$cname == "Germany",]
gerPreAtk = germany[germany$post_attack== 0,]
gerPostAtk = germany[germany$post_attack == 1,]
germany2 = germany[germany$post_attack,]
m1 = gerPreAtk$age
mean(m1)
var(m1)

m2 = gerPostAtk$age
mean(m2)
var(m2)


t.test(m1,m2, mu = 0, alternative = "two.sided", var.equal = FALSE, conf.level = 0.99, paired = FALSE)


#we cannot reject the null hypothesis because the Pvalue is more than the alpha when its 0.05, so it is not statistically significant

