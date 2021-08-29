library(car)
library(emmeans)

#rating lm
ratinglm=lm(data=anovata, logSales~anovata$Rating)
Anova(ratinglm, type=3)
#omnibus F sig, df(2, 1354)=8.838, p=<.001

#console lm
conslm=lm(data=anovata, logSales~anovata$Console)
Anova(conslm, type=3)
#sig, F(4, 1765)=31.475, p<.001

#publisher lm
publm=lm(data=anovata, logSales~anovata$Publisher)
Anova(publm, type=3)
#sig, F(17, 1280)=18.14, p<.001

#perspective lm
perslm=lm(data=anovata, logSales~anovata$Perspective)
Anova(perslm, type=3)
#sig, F(5, 1125)=11.709, p<.001

#genre lm
genlm=lm(data=anovata, logSales~anovata$Genre)
Anova(genlm, type=3)
#sig, F(7, 1202)=8.872, p<.001

#year lm
yrlm=lm(data=anovata, logSales~anovata$Year)
Anova(yrlm, type=3)
#not sig, F(6, 1763)=1.446, p=.193

#alllm
alllm2=lm(data=anovata, logSales~anovata$Rating+anovata$Console+anovata$Publisher+anovata$Perspective+
           anovata$Genre+anovata$Year)
summary(alllm)
Anova(alllm, type=3)

alllm=lm(data=anovata, logSales~Sequel+Re.release+Review+MaxPlayers+Online+Licensed+Accessory+
            Multiplatform+Rating+Console+Publisher+Perspective+
           Genre+Year)
Anova(alllm, type=3)


#emmeans
emtest=emmeans(object=alllm, specs=~Console, adjust="tukey")
emtest
pairs(emtest)

pairs(emmeans(object=alllm, specs=~Publisher, adjust="none"))
emmm=pairs(emmeans(object=alllm, specs=~Genre, adjust="none"))
list(emmm)
emmm2=pairs(emmeans(object=alllm, specs=~Perspective, adjust="none"))
