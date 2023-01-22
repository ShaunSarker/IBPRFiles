#2.4 code
ess = read.csv("terrorism-ess.csv", header = TRUE)
germany = ess[ess$cname == "Germany",]
germanyPreAtk = germany[germany$post_attack == 0,]
germanyPostAtk = germany[germany$post_attack == 1,]
i_im = israel$imm7
g_im = germany$imm7
g1 = germanyPreAtk$imm7
g2 = germanyPostAtk$imm7
error =  function(x) sd(x)/sqrt(length(x))
error(g1)
error(g2)


var(g_im)
sd(g_im)
sqrt(var(g_im))
mean(g_im)
t.test(g_im, alternative = "two.sided")
t.test(i_im,  alternative = "two.sided")
lb = function(x) (mean(x)-2*error(x))
hb = function(x) (mean(x)+2*error(x))
lb(g_im)
hb(g_im)
lb(i_im)
hb(i_im)
