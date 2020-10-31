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
	        list(v=0.0) )


# defining the names of the parameters we wish to monitor
parameter.names <- c( 'theta', 'v', 'c')


# defining the directory of OpenBUGS
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS"

# generating random samples using OpenBUGS
model1.exp <- bugs( data, inits1, model.file = "modelexp.txt",
                    parameters = parameter.names,n.chains = 1, 
			  n.iter = 1500, n.burnin=500, n.thin=1,  
			  bugs.directory = openbugs.dir, debug=F, program="OpenBUGS")

print(model1.exp,3) #model with 3 digits
#
names(model1.exp)

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
p0(model1.exp)

# trace plots - in a window with 3 rows and 2 columns
plot.trace( model1.exp,3,2)

# ergodic mean plots - in a window with 3 rows and 2 columns
plot.trace( model1.exp,3,2, ergodic=T)



# --------------------------------
# Plots without functions
# -------------------------------- 

#trace plot

par(mfrow=c(2,1))
# Trace plot of  "theta"
plot(model1.exp$sims.matrix[,"theta"],type='l',main="Trace plot of theta"
		,xlab="iterations",ylab="theta")

#acf plot
# Autocorrelation of theta 
acf(model1.exp$sims.matrix[,"theta"],main="ACF plot of theta",lag=100)







# --------------------------------
# Summarizing
# -------------------------------- 



#stats for theta
summary(model1.exp$sims.array[,1,'theta'])
quantile(model1.exp$sims.array[,1,'theta'],probs=0.95)   #to idio k me list k matrix

#stats for v=log(theta)
summary(model1.exp$sims.array[,1,'v'])
quantile(model1.exp$sims.array[,1,'v'],probs=0.95) 

#stats for c=1/theta
summary(model1.exp$sims.array[,1,'c'])
quantile(model1.exp$sims.array[,1,'c'],probs=0.95) 


#density plots

par(mfrow=c(2,2))
# Posterior density of "theta" 
plot(density(model1.exp$sims.matrix[,"theta"]),main="theta")

# Posterior density of "v" 
plot(density(model1.exp$sims.matrix[,"v"]),main="v")

# Posterior density of "c" 
plot(density(model1.exp$sims.matrix[,"c"]),,main="c")



# --------------------------------
# Ergotic mean plots
# -------------------------------- 

par(mfrow=c(3,1))

# Ergodic Trace plot of  "theta"
x<-model1.exp$sims.matrix[,"theta"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=colnames(model1.exp$sims.matrix)[1] )

x<-model1.exp$sims.matrix[,"v"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=colnames(model1.exp$sims.matrix)[2] )


x<-model1.exp$sims.matrix[,"c"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=colnames(model1.exp$sims.matrix)[3] )






# ------------------------------------
# Ergotic mean plots using CODA output
# ------------------------------------ 

modelexp.sim<-read.coda("output.odcCODAchain1", "output.odcCODAindex",start=501, quiet=FALSE) 	

par(mfrow=c(3,1))

# Ergodic Trace plot of  "theta"
x<-modelexp.sim[,"theta"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=" theta " )

x<-model1.exp$sims.matrix[,"v"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab=" log(theta) " )


x<-model1.exp$sims.matrix[,"c"]
plot( cumsum(x)/1:length(x), type='l',xlab="iterations",
		ylab="1 / theta")




