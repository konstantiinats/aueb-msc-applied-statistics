# loading the library/package
library(R2WinBUGS)
library(BRugs)

#Import data - sample size
Y<-c(0.4, 0.01, 0.2, 0.1, 2.1, 0.1, 0.9, 2.4, 0.1, 0.2)

# n defining the sample size
n<-length(Y)

#metasxhmatizw
data<-list(Y=Y, n=n)



# initial values
# should be a list with one list for each chain
#
inits1<-list(  
	        list(mu=0.0, tau=1.0) )


# defining the names of the parameters we wish to monitor
parameter.names <- c( 'mu', 'tau' , 's2')


# defining the directory of OpenBUGS
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS"

# generating random samples using OpenBUGS
model4.log <- bugs( data, inits1, model.file = "modellog.txt",
                    parameters = parameter.names,n.chains = 1, 
			  n.iter = 1500, n.burnin=500, n.thin=1,  
			  bugs.directory = openbugs.dir, debug=F, program="OpenBUGS")


print(model4.log,3) #model with 3 digits
#
names(model4.log)
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
p0(model4.log)

# trace plots - in a window with 3 rows and 2 columns
plot.trace( model4.log,3,2)

# ergodic mean plots - in a window with 3 rows and 2 columns
plot.trace( model4.log,3,2, ergodic=T)



# --------------------------------
# Plots without functions
# -------------------------------- 

#trace plot

par(mfrow=c(2,1))
# Trace plot of  "mu"
plot(model4.log$sims.matrix[,"mu"],type='l',main="Trace plot of mu"
		,xlab="iterations",ylab="mu")

# Trace plot of  "tau"
plot(model4.log$sims.matrix[,"tau"],type='l',main="Trace plot of tau"
		,xlab="iterations",ylab="tau")





par(mfrow=c(2,1))
#acf plots
acf(model4.log$sims.matrix[,"mu"],main="ACF plot of mu",lag=100)
acf(model4.log$sims.matrix[,"tau"],main="ACF plot of tau",lag=100)







# --------------------------------
# Summarizing
# -------------------------------- 


#density plots

par(mfrow=c(2,2))
# Posterior density of "mu" 
plot(density(model4.log$sims.matrix[,"mu"]),main="mu")

# Posterior density of "tau" 
plot(density(model4.log$sims.matrix[,"tau"]),main="tau")

# Posterior density of "s2" 
plot(density(model4.log$sims.matrix[,"s2"]),main="s2")




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

modellog.sim<-read.coda("output.odcCODAchain1", "output.odcCODAindex", quiet=FALSE) 	

head(modellog.sim)

par(mfrow=c(2,1))

# Ergodic Trace plot of  "mu"
x<-modellog.sim[,"mu"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=" mu " )

x<-modellog.sim[,"tau"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=" tau " )



