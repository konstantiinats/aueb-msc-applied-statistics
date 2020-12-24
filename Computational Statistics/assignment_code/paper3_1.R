###PAPER 3###
##1##


library(GoFKernel)

library(GoFKernel)

Ορίζω την αντίστροφη συνάρτηση.
f<-function(t){4*(1-t)/(^3}
F<-function(x) {integrate(f,lower=0,x)$value}
F.inv<-inverse(F,0,1)

Τρέχω τη προσομοίωση
xsim<-numeric(100)
u<-0
for (r in 1:100) {
	u<-runif(1)
	xsim[r]<-F.inv(u)
	}
