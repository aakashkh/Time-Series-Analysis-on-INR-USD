#correlogram of AR(1)
rho = function(k, alpha){
  alpha^k
}
layout(1:2)
plot(0:10, rho(0:10, 0.7), type = "b")
plot(0:10, rho(0:10, -0.7), type = "b")

#partial autocorrelation 
set.seed(1)
x = w= rnorm(100)
for(t in 2:100) x[t] = 0.7*x[t-1]+w[t]
layout(1:3)
plot(x, type = "l")
acf(x)
pacf(x)

# fitting AR
x.ar =  ar(x, method = "mle")
x.ar$order
x.ar$ar
x.ar
#confidence interval 
x.ar$ar + c(-2,2)*sqrt(x.ar$asy.var)

# Moving AVerage
set.seed(1)
b=  c(0.8,0.6,0.4)
x =w = rnorm(1000)
for(t in 4:1000){
  for(j in 1:3) x[t] = x[t]+b[j]*w[t-j]
  }
plot(x, type = "l")
acf(x)
x.ma = arima(x, order=c(0,0,3))
x.ma
