rm(list = ls())
import.jap = read.csv("ImportsfromJapan.txt", header = T)

#1.a plot time series
importjp.ts = ts(import.jap[,2], start=c(1988,1), freq = 12) 
plot(importjp.ts, main = "Japanese Import")
#there is increase in volatility. the variance increase over the time. there is a peak
#in year 2000, followed by a decrease in import. import starts to increase starting from 2002
# and onward.

#1.a ts on log
names(import.jap)
logimportjp.ts = ts(import.jap[,3], start = c(1988,1), freq = 12)
plot(logimportjp.ts, main = "log Japanese Import")

#1.2
fmonth = factor(import.jap$month)
time = 1: length(import.jap$logIMPJP)
attach(import.jap)
model1 = lm( logIMPJP~ 0 + time + I(time^2) + fmonth + c348 + s348, data = import.jap); summary(model1)
#residual analysis
resids = resid(model1)
spectrum(resids, span = 13)
plot(ts(resid(model1)))
acf(ts(resid(model1)))
pacf(ts(resid(model1)))

#use this model
model2 = lm( logIMPJP~ 0 + time + I(time^2) + I(time^3)+ 
               I(time^4) + fmonth + c348 + s348, data = import.jap)
summary(model2)
#residual analysis
resids2 = resid(model2)
spectrum(resids2, span = 13)
plot(ts(resid(model2)))
acf(ts(resid(model2)))
pacf(ts(resid(model2)))
qqnorm(ts(resid(model2)))

b = coef(model2)[5:16]
seas = exp(b - mean(b))
seas
plot(ts(seas), ylim = c(0.8,1.1), main = "seasonals")

#1.c

logreturn = ts(import.jap[,4], start=c(1988,1), freq = 12) 
plot(logreturn)
model3 = lm( dlogIMPJP~ 0 + fmonth + c348 + s348 + c432 + s432, data = import.jap)
summary(model3)
#residual analysis
resids3 = resid(model3)
spectrum(resids3, span = 13)
plot(ts(resid(model3)))
acf(ts(resid(model3)))
pacf(ts(resid(model3)))
qqnorm(ts(resid(model3)))

#seasonals
b = coef(model3)[1:12]
x = exp(b - mean(b))
s12 = 1
for(j in 2:12){
  xsub = x[j:12]
  s12 = s12*prod(xsub)
}
s12
s12 = s12^(1/12)
s12
s = c(rep(0,times = 12))
s[12]= s12
for (j in 1:11){
  xsub = x[1:j]
  s[j] = s[12]*prod(xsub)
}
s
plot(ts(s), ylim = c(0.8,1.1), main = "seasonals")


#1.d
#dont need month here
fmonth = factor(import.jap$month)
model4  = lm( logIMPJP~ time + I(time^2) +
                        I(time^3) + I(time^4), data = import.jap)
summary(model4)
model4.ts = ts(resid(model4))  
acf(model4.ts, lag = 36)
pacf(model4.ts, lag = 36)
arimamodel= arima(model4.ts, order = c(0,0,0), seasonal = list(order = c(0,1,0)))
acf(resid(arimamodel))
pacf(resid(arimamodel))
#pacf ar it cuts off at 3
arimamodel2= arima(model4.ts, order = c(3,0,0), seasonal = list(order = c(0,1,0)))
acf(resid(arimamodel2), lag = 36)
pacf(resid(arimamodel2), lag = 36)

#12 and 24. period = 12 #use this model
arimamodel3= arima(model4.ts, order = c(3,0,0), seasonal = list(order = c(2,1,0), period = 12))
#(pg9.)
acf(resid(arimamodel3), lag = 36)
pacf(resid(arimamodel3), lag = 36)
regresid2 = resid(model4)
spectrum(resid(arimamodel3), span = 13)
qqnorm(resid(arimamodel3))
plot(ts(resid(arimamodel3)))

arimapred = regresid2 - resid(arimamodel3)
monmeans = tapply(arimapred, month, mean)
seas = monmeans - mean(monmeans)
real.seas = exp(seas)
real.seas
plot(real.seas, type = "l", col = "green")
lines(s, col = "red")
lines(seas, col = "blue")
# legend(legend = c("first_model", "second_model", "third_model"), border = "black",
#        col = c("red", "blue", "green"))
#need to double check this 
comp = data.frame("first.model" = s, 
                  "second.model" = seas, 
                  "third.model" = real.seas)
comp
library("hwwntest")
bartlettB.test(resid(arimamodel3))
length(import.jap$logIMPJP)
1.36/sqrt(228/2)
#refer to 101917 notes
#102617


################################################################
#2.1
disney = read.csv("Disney.txt", header = T)
disney.ts = ts(disney[,3], start=c(1967,1), freq = 12) 
#identify(disney.ts) #use this to identify outlier
plot(disney.ts, main = "Disney Stock")


nrow(disney)
disney$d83 = rep(0, 600)
disney[83,4] = 1
disney$d93 = rep(0, 600)
disney[93,5] = 1
disney$d97 = rep(0, 600)
disney[97,6] = 1
disney$d250 = rep(0, 600)
disney[250,7] = 1
disney$d417 = rep(0, 600)
disney[417,8] = 1

#2.2
fmonth = factor(disney$month)
attach(disney)
names(disney)
model6 = lm( logreturn~ 0+fmonth + d83 + d93 +
               d97 + d250 + d417, data = disney); summary(model6)
#outlier = 83, 93, 250, 97, 417
# b = coef(model6)[1:12]
# seas = exp(b - mean(b))
# seas
b = coef(model6)[1:12]
x = exp(b - mean(b))
s12 = 1
for(j in 2:12){
  xsub = x[j:12]
  s12 = s12*prod(xsub)
}
s12
s12 = s12^(1/12)
s12
s = c(rep(0,times = 12))
s[12]= s12
for (j in 1:11){
  xsub = x[1:j]
  s[j] = s[12]*prod(xsub)
}
s
plot(ts(s), main = "seasonals")


#2.d
model7 = lm( logreturn~ fmonth + d83 + d93 +
               d97 + d250 + d417, data = disney); summary(model6)
disney.regresid.ts = ts(resid(model7))
acf(disney.regresid.ts)
pacf(disney.regresid.ts)

arimamodel3= arima(disney.regresid.ts, order = c(0,0,0), 
                   seasonal = list(order = c(4,0,0),
                                   period = 6))

acf(resid(arimamodel3))
pacf(resid(arimamodel3))
bartlettB.test(resid(arimamodel3))
length(disney$logreturn)
1.36/sqrt(600/2)
spectrum(resid(arimamodel3), span = 13)
qqnorm(resid(arimamodel3))
plot(ts(resid(arimamodel3)))


#2.e 1967 - 1991
which(disney$year == 1967)
which(disney$year == 1991)
subset.disney = disney[c(1:300),]
fmonth = factor(subset.disney$month)
attach(subset.disney)
names(subset.disney)
subsetdisney.ts = ts(subset.disney[,3], start=c(1967,1), freq = 12) 
outlier.subsetdisney <- tsoutliers::tso(subsetdisney.ts)

model6 = lm( logreturn~ 0+fmonth + d83 + d93 +
               d97 + d250, data = subset.disney); summary(model6)
#outlier = 83, 93, 250, 97, 417
# b = coef(model6)[1:12]
# seas = exp(b - mean(b))
# seas
b = coef(model6)[1:12]
x = exp(b - mean(b))
s12 = 1
for(j in 2:12){
  xsub = x[j:12]
  s12 = s12*prod(xsub)
}
s12
s12 = s12^(1/12)
s12
s = c(rep(0,times = 12))
s[12]= s12
for (j in 1:11){
  xsub = x[1:j]
  s[j] = s[12]*prod(xsub)
}
s
plot(ts(s), main = "seasonals")
sub.disney.regresid.ts = ts(resid(model6))
acf(sub.disney.regresid.ts)
pacf(sub.disney.regresid.ts)
arimamodel4= arima(sub.disney.regresid.ts, order = c(0,0,0), 
                   seasonal = list(order = c(0,0,2),
                                   period = 12))
acf(resid(arimamodel4))
pacf(resid(arimamodel4))
bartlettB.test(resid(arimamodel4))
#length(subset.disney$logreturn)
1.36/sqrt(300/2)
spectrum(resid(arimamodel4), span = 13)
qqnorm(resid(arimamodel4))
plot(ts(resid(arimamodel4)))
