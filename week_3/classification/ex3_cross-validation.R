library(GSE5859Subset)
data(GSE5859Subset)
#And define the outcome and predictors. To make the problem more difficult we will only consider autosomal genes:

y = factor(sampleInfo$group)
X = t(geneExpression)
out = which(geneAnnotation$CHR%in%c("chrX","chrY"))
X = X[,-out]

#Note, you will also need to load the following:
library(caret)

# Set the seed to 1 set.seed(1) then use the createFolds function in the caret package to create 10 folds of y.
# Q1: What is the 2nd entry in the fold 3?
set.seed(1)
idx = createFolds(y, k=10)
idx[[3]][2]

sapply(idx,function(ind) table(y[ind])) ##make sure every fold has 0s and 1s

# Q2: How many mistakes do we make on the test set? 
#     Remember it is indispensable that you perform the ttest on the training data.
library(class)
library(genefilter)
m=8
k=5
ind = idx[[2]]
pvals = rowttests(t(X[-ind,]),factor(y[-ind]))$p.val
ind2 = order(pvals)[1:m]
predict=knn(X[-ind,ind2],X[ind,ind2],y[-ind],k=k)
sum(predict!=y[ind])

# Q3: Now run the code for kNN and Cross Validation Exercises #2 for all 10 folds and keep track of the errors.
#     What is our error rate (number of errors divided by number of predictions) ?
result = sapply(idx,function(ind){
  pvals = rowttests(t(X[-ind,]),factor(y[-ind]))$p.val
  ind2 = order(pvals)[1:m]
  predict=knn(X[-ind,ind2],X[ind,ind2],y[-ind],k=k)
  sum(predict!=y[ind])
})
sum(result)/length(y)


ms=2^c(1:11)
ks=seq(1,9,2)
params = expand.grid(k=ks,m=ms)

# Q4: Now use apply or a loop to obtain error rates for each of these pairs of parameters.
#     Which pair of parameters minimizes the error rate?
errors = apply(params,1,function(param){
  k =  param[1]
  m =  param[2]
  result = sapply(idx,function(ind){
    pvals = rowttests(t(X[-ind,]),factor(y[-ind]))$p.val
    ind2 = order(pvals)[1:m]
    predict=knn(X[-ind,ind2],X[ind,ind2],y[-ind],k=k)
    sum(predict!=y[ind])
  })
  sum(result)/length(y)
})
params[which.min(errors),]
##make a plot and confirm its just one min:
errors = matrix(errors,5,11)
library(rafalib)
mypar(1,1)
matplot(ms,t(errors),type="l",log="x")
legend("topright",as.character(ks),lty=seq_along(ks),col=seq_along(ks))

# Q5: What is the minimum error rate?
pvals = rowttests(t(X),factor(y))$p.val
errors = apply(params,1,function(param){
  k =  param[1]
  m =  param[2]
  result = sapply(idx,function(ind){
    ind2 = order(pvals)[1:m]
    predict=knn(X[-ind,ind2],X[ind,ind2],y[-ind],k=k)
    sum(predict!=y[ind])
  })
  sum(result)/length(y)
})
min(errors)
##make a plot and compare to previous question
errors = matrix(errors,5,11)
library(rafalib)
mypar(1,1)
matplot(ms,t(errors),type="l",log="x")
legend("topright",as.character(ks),lty=seq_along(ks),col=seq_along(ks))


y = factor(as.numeric(format( sampleInfo$date, "%m")=="06"))
# Q6: What is the minimum error rate now?
errors = apply(params,1,function(param){
  k =  param[1]
  m =  param[2]
  result = sapply(idx,function(ind){
    pvals = rowttests(t(X[-ind,]),factor(y[-ind]))$p.val
    ind2 = order(pvals)[1:m]
    predict=knn(X[-ind,ind2],X[ind,ind2],y[-ind],k=k)
    sum(predict!=y[ind])
  })
  sum(result)/length(y)
})
min(errors)
##make a plot and confirm its just one min:
errors = matrix(errors,5,11)
library(rafalib)
mypar(1,1)
matplot(ms,t(errors),type="l",log="x")
legend("topright",as.character(ks),lty=seq_along(ks),col=seq_along(ks))

