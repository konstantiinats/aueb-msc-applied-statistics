####PAPER 2####
##4##


#we simulate from an exponential distribution
#we estimate lambda for every new dataset


n <- 95; lamda <- exp(fitexp$par)
prob <- numeric(1000)
for (r in 1:1000){
x <- rexp(n,lamda)
lambdastar<-optim(0.5, minusellexp, method='BFGS', dat=x, hessian=F)$par
fx<-function(x) {dexp(x,exp(lambdastar))}
prob[r] <- integrate(fx,0,10)$value
}

hist(prob,25,prob=T)
curve(dnorm(x,mean(prob),sd(prob)),col="blue",add=T)

#approaching normal distribution (CLT)
f<-function(x) {dexp(x,exp(fitexp$par))}
pr <- integrate(f,0,10)$value
pr


#probability estimation from our data

#se of probability
sd(prob)

