ess = read.csv("terrorism-ess.csv", header = TRUE)
israel = ess[ess$cname == "Israel",]
israeliMen = israel[israel$male == 1,]
israeliWomen = israel[israel$male == 0,]
m = nrow(israeliMen)
print(m)
w = nrow(israeliWomen)
nIs = nrow(israel)
print(nIs)
pHat = m/nIs
p = 0.50
print(pHat)
sdProp = sqrt(pHat * (1-pHat)/nIs)

sdPop = sqrt(p * (1-p)/1)
print(sdProp)
prop.test(129, n = nIs , p = 0.5, alternative = "two.sided",
          correct = FALSE)
#We cannot reject the null hypothesis as the P-value from the prop test is higher than 0.05 and 0.1 so the conclusion does not change



