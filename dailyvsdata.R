setwd("C:/Users/coffee/Downloads/Software/R/Cowpertwait/IF")
daily = read.csv("daily.csv")
daily_rates = daily[,2]
daily = rev(daily_rates)
daily.ts = ts(daily, freq=365)
plot.ts(daily.ts)
daily.decomp = decompose(daily.ts)

library(forecast)
# exponential smoothing
daily.es = HoltWinters(daily.ts, beta=0, gamma=0)
daily.es$coefficients
plot(daily.es)
plot(daily.es$fitted)
daily.es$SSE
daily.predict1 = forecast(daily.es)
plot(daily.predict1)

# removing trend and season, exponential smoothing
daily.decomp.es = HoltWinters(ts(daily.decomp$random[183:3452],frequency = 365), beta = 0, gamma = 0)
daily.decomp.es$coefficients
# For checking - 
daily.decomp.es$x-daily.decomp$random[183:3452]
#final
plot(daily.decomp.es$fitted[,1]+ts(daily.decomp$trend[183:3452],frequency = 365)+ts(daily.decomp$seasonal[183:3452], frequency = 365))
daily_afterSmoothing = daily.decomp.es$fitted[,1]+ts(daily.decomp$trend[183:3452],frequency = 365)+ts(daily.decomp$seasonal[183:3452], frequency = 365)
daily.decomp.es$SSE
#par(mfrow =c(3,1))
daily.predict2 = forecast(daily.decomp.es$fitted[,1])
daily.predict3 = forecast(daily.decomp.es)
daily.predict4 = forecast(daily_afterSmoothing)
plot(daily.predict2)
plot(daily.predict3)
plot(daily.predict4)

#par(mfrow =c(4,1))
data.hw4 = HoltWinters(daily.ts, seasonal = "additive")
plot(data.hw4)
data.predict1 = forecast(data.hw4)
plot(data.predict1)


daily.hw4 = HoltWinters(daily.ts, seasonal = "additive")
plot(daily.hw4)
data.predict1 = forecast(daily.hw4)
plot(data.predict1)
 # plot(daily.hw4$fitted)

#par(mfrow =c(2,1))
plot(daily.hw4)
plot(data.hw4)

#par(mfrow =c(1,1))
plot(daily.hw4$fitted)
plot(data.hw4$fitted)

