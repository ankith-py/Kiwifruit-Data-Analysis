kiwi <- read.csv("D:/Kiwi_data.csv")

library(rsm)

Ultra <- kiwi$X1
Temp <- kiwi$X2
Trt_Time <- kiwi$X3
Concen <- kiwi$X4

bacteria <- kiwi$bacteria
yeast <- kiwi$yeast
firmness <- kiwi$firmness
resp.rate <- kiwi$resp.rate

firmness.mod <- rsm(firmness ~ SO(Ultra, Temp, Trt_Time, Concen))
summary(firmness.mod)

firmness.mod2 <- rsm(firmness ~ FO(Ultra, Trt_Time, Concen) + 
                  TWI(Ultra, Trt_Time) + PQ(Concen))

summary(firmness.mod2) 

anova(firmness.mod2, firmness.mod)

par(mfrow=c(1,3))
contour(firmness.mod2, ~ Ultra + Trt_Time + Concen,at=summary(firmness.mod2)$canonical$xs, image=TRUE)

par(mfrow=c(1,3))
persp(firmness.mod2, ~ Ultra + Trt_Time + Concen, col= "gold1",
      at=summary(firmness.mod2)$canonical$xs)

