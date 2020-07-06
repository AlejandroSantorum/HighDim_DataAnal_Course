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

# Q1: Treating the data generated above as the population, if we know someone is 176 cm tall,
#     what it the probability that this person is a woman:  Pr(𝑌=1|𝑋=176)=E(𝑌|𝑋=176) ?
mean(y[x==176])

# Now make a plot of𝐸(𝑌|𝑋=𝑥) for x=seq(160,178) using the data generated in Conditional Expectation Exercises #1.
xs = seq(160,178)
pr =sapply(xs,function(x0) mean(y[x==x0]))
plot(xs,pr)
abline(h=0.5)
# Q2: Suppose for each height x you predict 1 (female) if Pr(𝑌|𝑋=𝑥)>0.5 and 0 (male) otherwise.
#     What is the largest height for which you predict female ?
abline(v=168)
