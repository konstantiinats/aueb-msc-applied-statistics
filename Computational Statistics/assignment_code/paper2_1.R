###PAPER 2###

##1##

y<-c(2.1,4,2.6,1.5,2.5,4,2,
     3.4,4.1,3.6,4.7,2.5,4,2.7,
     4.25,5,3.6,4.7,3.4,5.25,2.75,
     5.6,5.5,6.4,7.2,4.2,6.1,3.4,
     6.4,5.7,6.8,7.25,5.9,6.5,4.2,
     7.3,6.5,7.5,8.1,6.25,6.9,4.3,
     8.5,7.25,7.5,8.5,7.3,7,4.9,
     8.75,7.3,8.25,9.2,7.5,8.45,6.25,
     8.9,7.5,8.5,9.5,7.8,9.25,7,
     9.5,8.2,10.4,10.7,8.3,10.1,9,
     9.75,8.5,10.75,11.5,8.3,10.2,9.25,
     10,9.75,14.25,10.25,12.75,10.7,
     10.4,11,14.5,12.9,14.6,
     10.4,11.2,14.3,
     16,15,
     19,16.5)




##a=ln(a) k=ln(k) l=ln(ë)


#minus log likelihood function
negloglikgengamma<-function(thet,dat){
				a<-thet[1]
				k<-thet[2]
				l<-thet[3]
				n<-length(dat)
				-(n*a+n*exp(k)*l-n*log(gamma(exp(k)))+(exp(a)*exp(k)-1)
				*sum(log(dat))-exp(l)*sum(dat^exp(a)))
				}




negloglikgengamma(c(-0.2,-3,-3.5),y)#dokimi a douleyei h loglik

fitggamma<-optim(c(-100,-100,-100),negloglikgengamma,hessian=TRUE,method="BFGS",dat=y)




ggammamle<-cbind(est=fitggamma$par,se=sqrt(diag(solve(fitggamma$hessian))))
ggammamle 


#95% CI for log(a)=è1
CI<-cbind(LCI=ggammamle[1,1]-2*ggammamle[1,2],UCI=ggammamle[1,1]+2*ggammamle[1,2])
exp(CI)

#percentile of t distribution


