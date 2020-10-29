###PAPER 1###
##2##

x<-c(1.77,-0.23,2.76,3.80,3.47,56.75,-1.34,4.24,-2.44,3.29,3.71,
	-2.40,4.53,-0.07,-1.05,-13.87,-2.53,-1.75,0.27,43.21)



theta0<-c(-11,-1,0,1.5,4,4.7,7,8,38,mean(x))

theta<-c()

for (thet0 in theta0) {

eps<-1
while (eps > 1e-6) {
elldash<-2*sum((x-thet0)/(1+(x-thet0)^2))
ellddash<-2*sum(((x-thet0)^2-1)/(1+(x-thet0)^2)^2)
thet<-thet0-elldash/ellddash
eps<-abs(thet-thet0)
thet0<-thet
}

theta[i]<-theta0

}

A<-cbind(theta0,theta)
A

