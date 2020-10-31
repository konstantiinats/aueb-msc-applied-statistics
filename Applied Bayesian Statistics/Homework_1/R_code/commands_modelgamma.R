# loading the library/package
library(R2WinBUGS)
library(BRugs)

#Import data - sample size
data<-list(Y=c(0.4, 0.01, 0.2, 0.1, 2.1, 0.1, 0.9, 2.4, 0.1, 0.2),n=n)


# n defining the sample size
n<-length(data$Y)


# initial values
# should be a list with one list for each chain
#
inits1<-list(  
	        list(a=1.0, b=1.0) )


# defining the names of the parameters we wish to monitor
parameter.names <- c( 'a', 'b')


# defining the directory of OpenBUGS
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS"

# generating random samples using OpenBUGS
model2.gamma <- bugs( data, inits1, model.file = "modelgamma.txt",
                    parameters = parameter.names,n.chains = 1, 
			  n.iter = 1500, n.burnin=500, n.thin=1,  
			  bugs.directory = openbugs.dir, debug=F, program="OpenBUGS")

print(model2.gamma,3) #model with 3 digits
#
names(model2.gamma)

#
# function for creating trace and ergodic mean plots
# 2013 April by Ioannis Ntzoufras
#
plot.trace <- function( bugs.object, nrow=5, ncol=NULL, ergodic=FALSE){ 
	mcmc.output<-bugs.object$sims.matrix 
	n.iter <- nrow(mcmc.output)    #arithmos iterations/vhmatwn-simulations
	n.par  <- ncol(mcmc.output)    #arithmos parametrwn
	if (is.null(ncol)) ncol <- (n.par %/% nrow)+1*( (n.par %% nrow)!=0 )

	par(mfcol=c(nrow,ncol) )
	if (ergodic){ 
		for (k in 1:n.par){ 
			plot( cumsum(mcmc.output[,k])/1:n.iter, type='l', main=colnames(mcmc.output)[k]) }
		
	}else{
		for (k in 1:n.par){ plot( mcmc.output[,k], type='l', main=colnames(mcmc.output)[k]) }
	}
}


#
# function for checking the centrality of zero
# 2013 April by Ioannis Ntzoufras
# 
p0 <- function( bugs.object, digits=3){ 
	mcmc.output<-bugs.object$sims.matrix 
	n.iter <- nrow(mcmc.output)
	n.par  <- ncol(mcmc.output)
	mcmc.output<-mcmc.output[ , -n.par]     #vgazei to Deviance
	temp<-apply( mcmc.output < 0, 2, mean)
	res <- pmin( temp, 1-temp)  #min gia kathe stoixeio (temp,1-temp) 
	return( round(res,digits) )
}

# calculating the probability of zero to be central in the posterior densities
p0(model2.gamma)

# trace plots - in a window with 3 rows and 2 columns
plot.trace( model2.gamma,3,2)

# ergodic mean plots - in a window with 3 rows and 2 columns
plot.trace( model2.gamma,3,2, ergodic=T)



# --------------------------------
# Plots without functions
# -------------------------------- 

#trace plots
par(mfrow=c(2,1))
# Trace plot of  "a"
plot(model2.gamma$sims.matrix[,"a"],type='l',main="Trace plot of a"
		,xlab="iterations",ylab="a")

# Trace plot of  "b"
plot(model2.gamma$sims.matrix[,"b"],type='l',main="Trace plot of b"
		,xlab="iterations",ylab="b")






#acf plots
par(mfrow=c(2,1))
acf(model2.gamma$sims.matrix[,"a"],main="ACF plot of a",lag=100)
acf(model2.gamma$sims.matrix[,"b"],main="ACF plot of b",lag=100)







# --------------------------------
# Summarizing
# -------------------------------- 



#stats for theta
summary(model2.gamma$sims.array[,1,'a'])
quantile(model2.gamma$sims.array[,1,'a'],probs=0.95)   #to idio k me list k matrix

#stats for v=log(theta)
summary(model2.gamma$sims.array[,1,'b'])
quantile(model2.gamma$sims.array[,1,'b'],probs=0.95) 




#density plots

par(mfrow=c(1,2))
# Posterior density of "a" 
plot(density(model2.gamma$sims.matrix[,"a"]),main="a")

# Posterior density of "b" 
plot(density(model2.gamma$sims.matrix[,"b"]),main="b")




# --------------------------------
# Ergotic mean plots
# -------------------------------- 

par(mfrow=c(2,1))

# Ergodic Trace plot of  "theta"
x<-model2.gamma$sims.matrix[,"a"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab="a" )

x<-model2.gamma$sims.matrix[,"b"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab="b" )







# ------------------------------------
# Ergotic mean plots using CODA output
# ------------------------------------ 

modelgamma.sim<-read.coda("output.odcCODAchain1", "output.odcCODAindex",start=501, quiet=FALSE) 	

par(mfrow=c(2,1))

# Ergodic Trace plot of  "a"
x<-modelgamma.sim[,"a"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=" a " )

x<-modelgamma.sim[,"b"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=" b " )









