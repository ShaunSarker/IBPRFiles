ess = read.csv("terrorism-ess.csv", header = TRUE)
germany = ess[ess$cname == "Germany",]
germanyPreAtk = germany[germany$post_attack == 0,]
germanyPostAtk = germany[germany$post_attack == 1,]
g_im = germany$imm7
g1 = germanyPreAtk$imm7
g2 = germanyPostAtk$imm7
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
median(g1)
range(g1)
mean(g1)
getmode(g1)
median(g2)
range(g2)
getmode(g2)
mean(g2)
quantile(g_im)
quantile(g1)
quantile(g2)
hist(g1, main = "Relationship between pre attack and immigration", xlab = "Immigration Score", xlim = c(0,11),ylim =c(0.00, 0.30) , col = "blue", freq = FALSE)
hist(g2, main = "Relationship between post attack and immigration Stance", xlab = "Immigration Score", xlim = c(0,11),ylim =c(0.00, 0.30) , col = "blue", freq = FALSE)
hist(g_im, main = "German's Thoughts on Immigration", xlab = "Immigration Score", xlim = c(0,11),ylim =c(0.00, 0.30) , col = "blue", freq = FALSE)
std.e <- function(x) sd(x)/sqrt(length(x))
std.e(g1)
std.e(g2)
