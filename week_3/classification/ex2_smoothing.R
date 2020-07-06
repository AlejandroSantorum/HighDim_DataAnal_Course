n = 10000
set.seed(1)
men = rnorm(n,176,7) #height in centimeters
women = rnorm(n,162,7) #height in centimeters
y = c(rep(0,n),rep(1,n))
x = round(c(men,women))
##mix it up
ind = sample(seq(along=y))
y = y[ind]
x = x[ind]

set.seed(5)
N = 250
ind = sample(length(y),N)
Y = y[ind]
X = x[ind]

fit=loess(Y~X)
# Q1
predict(fit,newdata=data.frame(X=168))

##Here is a plot
xs = seq(160,178)
Pr =sapply(xs,function(x0) mean(Y[X==x0]))
plot(xs,Pr)
fitted=predict(fit,newdata=data.frame(X=xs))
lines(xs,fitted)


##plot plots are optional
set.seed(5)
B = 1000
N = 250
xs = seq(160,178)
plot(xs,xs,ylim=c(0,1),type="l")
res = replicate(B,{
  ind = sample(length(y),N)
  Y = y[ind]
  X = x[ind]
  fit=loess(Y~X)
  ##optional plots
  fitted=predict(fit,newdata=data.frame(X=xs))
  lines(xs,fitted)
  estimate = predict(fit,newdata=data.frame(X=168))
  return(estimate)
})

library(rafalib)
# Q2
popsd(res)

