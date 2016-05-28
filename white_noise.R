# white noise
set.seed(1)
w = rnorm(100)
plot(w, type = "l")

# normal distribution
x = seq(-3,3,length=1000)
hist(rnorm(100), probability = T)
points(x, dnorm(x), type ="l")

# autocorrelation of white noise
set.seed(2)
acf(rnorm(100))

# random walk
x=w=rnorm(1000)
for(t in 2:1000) x[t]=x[t-1]+w[t]
plot(x, type = "l")
acf(x)
acf(diff(x))
