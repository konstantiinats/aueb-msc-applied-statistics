###PAPER 2###
##2##



######################
#pdf of our log distribution
gg.pdf<-function(x){
				a<-exp(ggammamle[1,1]) 
				k<-exp(ggammamle[2,1])
				l<-exp(ggammamle[3,1])
				f<-(a*(l^k)*(x^(a*k-1))*exp(-l*(x^a)))/gamma(k)
				return(f)
				}


plot(gg.pdf,y) #einai "1-1"


#minus log likelihood function for alpha
negloglikgengamma<-function(alpha,dat) {
				
				a<-alpha
				k<-ggammamle[2,1]
				l<-ggammamle[3,1]
				n<-length(dat)
				-(n*a+n*exp(k)*l-n*log(gamma(exp(k)))+(exp(a)*exp(k)-1)
				*sum(log(dat))-exp(l)*sum(dat^exp(a)))
				}


aa<-seq(-20,2,0.1)
plot(negloglikgengamma,aa)






#library for inverse function
library(GoFKernel)



thetastar <- numeric(800)
thet<- 0
for (i in 1:800){	
	#generating random samples from my distribution
	xstar<-numeric(95)
	f.x<-0
	u<-0
	for (j in 1:95) {
		u<-runif(1)
		F.x<-function(x) {integrate(gg.pdf,lower=0,x)$value}
		f.x<-inverse(F.x,lower=0)
		xstar[j]<-f.x(u)
		}
	#ektimhsh gia to è1
xstar<-runif(95,0,20)
	thet<-optim(-100,negloglikgengamma,hessian=TRUE,method="BFGS",dat=xstar)
	thetastar[i]<-thet$par
	}



plot(negloglikgengamma(aa,xstar),aa)
#for the confidence intervals estimation I use the percentiles of thetastar cdf


up<-quantile(thetastar,  probs = 0.975)
lr<-quantile(thetastar,  probs = 0.025)


CIBoot<-cbind(LCI=lr,UCI=up)  #0.47 polu ektos twn diasthmatwn bootstrap - oxi kalh prosarmogh
CIBoot


exp(CIBoot)
exp(CI)
ggammamle[1,1]
