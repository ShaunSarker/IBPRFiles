install.packages("seewave")
library(seewave)
poll = read.csv("poll-dk-2019.csv", header = TRUE)
dim(poll)
poll$ToElecDay <- as.Date("2019-06-04")-as.Date(poll$poll_date)
ess[ess$cname == "Germany",]
ld = subset(poll, ToElecDay == 0)
yg = poll[poll$pollingfirm == "YouGov",]

ygld =  subset(ld, pollingfirm == "YouGov")
vox = poll[poll$pollingfirm == "Voxmeter",]
voxld = subset(ld, pollingfirm == "Voxmeter")
#Method to systamtically subtract actual and estimate seems rather difficult

ygAccurate = mean(abs(c(yg$party_a - yg$party_a_res, yg$party_aa - yg$party_aa_res, yg$party_o - yg$party_o_res, yg$party_v - yg$party_v_res)))

ygAccurate

ygldAccurate = mean(abs(c(ygld$party_a - ygld$party_a_res, ygld$party_aa - ygld$party_aa_res, 
                         ygld$party_o - ygld$party_o_res, ygld$party_v - ygld$party_v_res)))

ygldAccurate


voxAccurate = mean(abs(c(vox$party_a - vox$party_a_res, vox$party_aa - vox$party_aa_res, vox$party_o - vox$party_o_res, vox$party_v - vox$party_v_res)))
voxAccurate

voxldAccurate =  mean(abs(c(voxld$party_a - voxld$party_a_res, voxld$party_aa - voxld$party_aa_res, voxld$party_o - voxld$party_o_res, voxld$party_v - voxld$party_v_res)))
voxldAccurate


aBias <- mean(abs(poll$party_a - poll$party_a_res))
oBias = mean(abs(poll$party_o - poll$party_o_res))
vBias = mean(abs(poll$party_v - poll$party_v_res))
aaBias = mean(abs(poll$party_aa - poll$party_aa_res))
aBias
oBias
vBias
aaBias
rms(poll$party_a - poll$party_a_res)
rms(poll$party_o - poll$party_o_res)
rms(poll$party_aa - poll$party_aa_res)
rms(poll$party_v - poll$party_v_res)



percentageError = function(estimate, result){
  a = (abs(estimate - result)/result) * 100 
}

ape = percentageError(poll$party_a, poll$party_a_res)
ope = percentageError(poll$party_o, poll$party_o_res)
vpe = percentageError(poll$party_v, poll$party_v_res)
aape = percentageError(poll$party_aa, poll$party_aa_res)
mean(ape)
mean(ope)
mean(vpe)
mean(aape)
ygAPe =  percentageError(yg$party_a, yg$party_a_res)
ygOPe = percentageError(yg$party_o, yg$party_o_res)
ygvPe = percentageError(yg$party_v, yg$party_v_res)
ygAApe = percentageError(yg$party_aa, yg$party_aa_res)

vApe = percentageError(vox$party_a, vox$party_a_res)
vOpe = percentageError(vox$party_o, vox$party_o_res)
vVpe = percentageError(vox$party_v, vox$party_v_res)
vAApe = percentageError(vox$party_aa, vox$party_aa_res)


overError = function(poll1,poll2,poll3,poll4){
  e = (poll1 + poll2 + poll3 + poll4)/4
  
}

partyPoll = overError(ape, ope, vpe, aape)
as.table(partyPoll)
mean(partyPoll)
ygPartyPoll = overError(ygAPe, ygOPe, ygvPe, ygAApe)
voxPartyPoll = overError(vApe, vOpe, vVpe, vAApe)

regression <- lm(partyPoll ~ poll$ToElecDay)
summary(m1)

par(mfrow=c(1,3))
plot(poll$ToElecDay, partyPoll, 
     xlim = rev(c(-3, 153)),
     ylim = c(-1, 45),
     main = "Prediction of Error",
     xlab = "Days till election",
     ylab = "Percentage error of party polls",
     pch = 19,
     col = "Red")
abline(16.7482, 0.1648)

plot(yg$ToElecDay, ygPartyPoll, 
     xlim = rev(c(-2, 149)),
     ylim = c(-0.5, 42),
     main = "Prediction of error",
     xlab = "Days till election",
     ylab = "Percentage Error of YouGov Polls",
     pch = 19,
     col = "purple")
abline(16.7482, 0.1648)

plot(vox$ToElecDay, voxPartyPoll, 
     xlim = rev(c(-2, 149)),
     ylim = c(-0.5, 42),
     main = "Prediction of Error",
     xlab = "Days till election",
     ylab = "mPecentage Error of Vox Polls",
     pch = 19,
     col = "green")
abline(16.7482, 0.1648)




ppSize = lm(partyPoll ~ poll$n)
summary(ppSize)

par(mfrow=c(1,3))
plot(poll$n, partyPoll,  xlim = c(900,6000),
     ylim = c(0, 42),
     main = "Size of Poll relationship with Error Percentage of Party Polls",
     xlab = "Size of Poll",
     ylab = "Percentage Error of Party Poll",
     pch = 20, 
     col = "purple")
abline(24.207605, -0.001162)


smallPolls =  poll[poll$n < 3000,]
app = percentageError(smallPolls$party_a, smallPolls$party_a_res)
opp = percentageError(smallPolls$party_o, smallPolls$party_o_res)
vpp = percentageError(smallPolls$party_v, smallPolls$party_v_res)
aapp = percentageError(smallPolls$party_aa, smallPolls$party_aa_res)

smallPollsPercentageError = (app + opp + vpp + aapp)/4  
smallPollsPercentageError


superSmallpp = lm(smallPollsPercentageError ~ smallPolls$n)
summary(superSmallpp)

plot(smallPolls$n, smallPollsPercentageError,
     main = "Error % and size of poll",
     xlab = "Poll size",
     ylab = "Overall prediction error in %",
     pch = 20, 
     col = "darkgreen")
abline(9.647707, 0.012577)



