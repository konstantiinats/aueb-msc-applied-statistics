
##PAPER 1##
#question 1


#function 1 using the formation definition


vardf<-function(n,mn=0,vr=1) {
	y<-rnorm(n,mn,vr)
	f<-function(x) x*dnorm(x,mn,vr)
	exp<-integrate(f, min(y) , max(y))
	f<-function(x) (x^2)*dnorm(x,mn,vr)
	ropi<-integrate(f, min(y) , max(y))
	exp<-as.numeric(exp[1])
	ropi<-as.numeric(ropi[1])
	var<-ropi-exp^2
	return(var)
        }





#function 2 based on the hand calculation equation

varhc<-function(n,mn=0,vr=1) {
	x<-rnorm(n,mn,vr)
	v<-0
	m<-mean(x)
	var<-((x-m)^2)/(n-1)
	return(var)
	}





#for different variations

v<-seq(200,20,50)
for(i in 1:v) {
    x[i]<-vardf(10000,1000,i)
    y[i]<-varhc(10000,1000,i)
    }


A<-cbind(v,sqrt(x),sqrt(y))
colnames(A)<-c("sd","formation definition","hand calculation equation")
A








