###PAPER 3###
##2##


library(GoFKernel)

#we want to simulate y for a given x
#eg x=0.05
x<-0.05

f<-function(t){6*x*(1-t-x)/(1-t)^3}
F<-function(x) {integrate(f,lower=0,x)$value}
F.inv<-inverse(F,0,0.95)

z<-seq(0,0.95,0.01)
Fx<-numeric(length(z))
for (i in 1:length(z) ) {
	Fx[i]<-F(z[i])
	}
plot(z,Fx,type='l',main="plot of cdf")

#h f(x) "1-1" so inversable
#I can use inversion method

ysim<-numeric(100)
u<-0
for (r in 1:100) {
	u<-runif(1)
	ysim[r]<-F.inv(u)
	}


xsim
