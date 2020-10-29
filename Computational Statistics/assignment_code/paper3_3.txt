ysim<-numeric(100)
u<-0

u<-0
x<-0
for (r in 1:100) {
	x<-simx[r]
        	f<-function(t){6*x*(1-t-x)/(1-t)^3}
F<-function(x) {integrate(f,lower=0,x)$value}
F.inv<-inverse(F,0,1-x)

	u<-runif(1)
	ysim[r]<-F.inv(u)
	}



cor(xsim,ysim)
sqrt(6)/6
