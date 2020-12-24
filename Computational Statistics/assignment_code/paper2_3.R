###PAPER 2###
##3##

#fitting an exponential distribution
#Initially, we estimate the parameter
#still using the parameterisation è3=log(ë)
#lambda=log(l)

ellexp <- function(lambda,dat) {
 #exponential log-likelihood 
 n <- length(dat)
 -n*log(lambda)+lambda*sum(dat) 
} 
#plot the log-likelihood over [-5, 5]
lambda <- seq(-5,5,0.1) 
 #option 1: using vectorisation
 plot(lambda, ellexp(lambda,dat=y), type='l', ylab='log-likelihood')



minusellexp <- function(lambda,dat) {
 #exponential minus log-likelihood
 n <- length(dat)
 -n*lambda+exp(lambda)*sum(dat)
}




fitexp <- optim(0.5, minusellexp, method='BFGS', dat=y, hessian=T)
fitexp

list(estim=fitexp$par,st.er.=sqrt(solve(fitexp$hessian)))




############################
#second lazy way#
 
minusellexp.lazy <- function(lambda,dat) {
 #"lazy" exponential minus log-likelihood, using built-in exponential pdf 
 -sum(dexp(dat,rate=lambda,log=T))
}


#find MLE and se numerically
fitexp2.lazy <- optim(0.5, minusellexp.lazy, method='BFGS', dat=testdat, hessian=T)
 #same as fitexp2

############################



#LR test
#LR=-2[l(è1)-l(è2)]
#LR=-2(-minusellexp(è1)-(-negloglikgengamma(è2)), where è2 a vector



lrt <- numeric(1000)
for (r in 1:1000) {
x<-sample(y, replace=T)
fit1<-optim(c(-100,-100,-100),negloglikgengamma,method="BFGS",dat=x)
fit2<-optim(0.5,minusellexp,method="BFGS",dat=x)
l1<--fit1$value
l2<--fit2$value
lrt[r] <- -2*(l2-l1)
}


###fitting x^2
hist(lrt,25,prob=TRUE)
curve(dchisq(x,2),col="blue",add=T)
###


#comparison of simulated (exact) and ×2,2
#(asymptotic) cutoff points
rbind(simulated=quantile(lrt,prob=c(0.8,0.9,0.95,0.99)),
chisq=qchisq(c(0.8,0.9,0.95,0.99),2))




#lrt estimation from our data
lr.t<- -2*(fitggamma$value-fitexp$value)
lr.t

#or
lr.test(minusellexp(fitexp$par,x),negloglikgengamma(fitggamma$par,x),0.05,2)

install.packages('extRemes')
library(extRemes)


#Monte Carlo p value
(sum(lrt>lr.t) +sum(lrt<-lr.t) )/1000

#we accept Ho

(sum(lrt>63.472) +sum(lrt<-63.472) )/1000

