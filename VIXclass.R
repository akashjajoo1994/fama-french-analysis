
install.packages("MASS")
library(MASS)

VIX2 <- read.table(file="VIX2.txt", header=T)
open <- VIX2$open
date1 <-VIX2$date
close <- VIX2$close
ctc <- VIX2$ctc
oto <- VIX2$oto
otc <- VIX2$otc
cto <- VIX2$cto
D1 <- VIX2$d1
D2 <- VIX2$d2
D3 <- VIX2$d3
D4 <- VIX2$d4
D5 <- VIX2$d5

ctc <- ctc*100
otc <- otc*100
cto <- cto*100
oto <- oto*100


y1 <-cbind(mean(ctc), sd(ctc), skewness(ctc), kurtosis(ctc), max(ctc), min(ctc))
y2 <-cbind(mean(otc), sd(otc), skewness(otc), kurtosis(otc), max(otc), min(otc))
y3 <-cbind(mean(cto), sd(cto), skewness(cto), kurtosis(cto), max(cto), min(cto))
y4 <-cbind(mean(oto), sd(oto), skewness(oto), kurtosis(oto), max(oto), min(oto))
y <-rbind(y1, y2, y3, y4)  

lret <- cto
mn <- mean(lret)
lret <- lret-mn

regret <- lm(lret~D1+D2+D3+D4+D5-1)
summary(regret)

