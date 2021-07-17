
# 5.3.1 The Validation Set Approach
library(ISLR)
set.seed(1)
train=sample(392,196) # sample():train/test

lm.fit = lm(mpg~horsepower, data=Auto, subset=train)

attach(Auto) # 데이터셋 이름 생략하고 안쓰겠다. 변수명만 쓰겠다.
mean((mpg-predict(lm.fit, Auto))[-train]^2) # -train index below selects only the observations that are not in the training set.

lm.fit2 = lm(mpg~poly(horsepower, 2), data= Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower,3),data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# If we choose a different training set instead -> get different errors on the validation set.
set.seed(2)
train=sample(392,196)
lm.fit_2=lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit_2, Auto))[-train]^2)

lm.fit2_2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2_2,Auto))[-train]^2)

lm.fit3_2 = lm(mpg~poly(horsepower,3),data=Auto, subset=train)
mean((mpg-predict(lm.fit3_2,Auto))[-train]^2)


# 5.3.2 Leave-One-Out Cross-Validation
glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit)


lm.fit_3=lm(mpg~horsepower ,data=Auto)
coef(lm.fit)

library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto,glm.fit) # default k= n개, 따로 안넣어도 된다.
cv.err$delta

# use for() loop for polynomial regression
cv.error = rep(0,5)
for (i in 1:5){
glm.fit2=glm(mpg~poly(horsepower,i), data=Auto)
cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}

cv.error

# 5.3.3 K-Fold Cross-Validation
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
# cv.glm() : function can also be used to implement k-fold CV.

cv.error.10
# 5.3.4 The BootStrap

# Create function alpha.fn, which takes as input the (X,Y) data as well as a vector indicating which observations should be used to estimate alpha.
alpha.fn=function(data,index){
X=data$X[index]
Y=data$Y[index]
return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))) 
}

# it returns an estimate for alpha based on applying (5.7) to the observatinos indexed by the argument index.
alpha.fn(Portfolio, 1:100)


set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T)) 
# sample() : function to randomly select 100 observations from the large 1 to 100, with replacement.

# R=1,000 bootstrap estimate for alpha
boot(Portfolio ,alpha.fn,R=1000)


# [Estimating the Accuracy of a Linear Regression Model]

# boot.fn(), which takes in the Auto data set as well as a set of indices for the observations, 
# and returns the intercept and slope estimates for the linear regression model.
boot.fn=function(data,index)return(coef(lm(mpg~horsepower ,data=data,subset=index)))
boot.fn(Auto ,1:392)


set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))

boot.fn(Auto,sample(392,392,replace=T))


# standard errors of 1,000 bootstrap estimates for the intercept and slope terms.
boot(Auto, boot.fn, 1000)

# summary() also can compute the standard errors
summary(lm(mpg~horsepower ,data=Auto))$coef

# Summary 값과 Bootstrap 결과와 다소 다름: 이유-> 책

boot.fn=function(data,index)coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))

set.seed(1)
boot(Auto, boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
