
library(R2WinBUGS)
library(BRugs)


#Importing data

library(foreign)
sm<-read.spss("C://R//14_supermarkets.sav",use.value.label=TRUE,to.data.frame=TRUE)

#### grouping shop categories with low frequencies
x<-summary(sm$shop)
x<-x[x<=10]
x<-names(x)

sm$shop<-as.character(sm$shop)

for (j in 1:length(x)) { for (i in 1:dim(sm)[1]){
  if (is.na(sm$shop[i])==FALSE){  
   if (sm$shop[i]==x[j]){ sm$shop[i]<-"Others"}
  }
}}

sm$shop<-as.factor(sm$shop)
summary(sm$shop)
rm(x)
####


sm$spen_yr<-as.character(sm$spen_yr)
sm$spen_yr<-as.numeric(sm$spen_yr)

sm$car<-as.numeric(sm$car)
sm$car[sm$car==1]<-0
sm$car[sm$car==2]<-1

sm$shop<-as.numeric(sm$shop)
sm$distance<-as.numeric(sm$distance)

sm$sex<-as.numeric(sm$sex)
sm$sex[sm$sex==1]<-0
sm$sex[sm$sex==2]<-1

# n defining the sample size
n<-nrow(sm)

detach(sm)
# we attaching the estriol.ex in the main workspace (otherwize R2WinBUGS will not be able to see them)
attach(sm)

# defining the names of the data objects
data.names<-c(names(sm) ,'n')


# defining the names of the parameters we wish to monitor
parameter.names <- c( 's', 'tau' , 'alpha1', 'alpha2' , 'alpha3' , 'm' , 'alpha4' , 'alpha5' 
                         , 'alpha6',  'alpha7',  'alpha8',  'alpha9',  'alpha10',  'alpha11',  )


# defining the directory of OpenBUGS
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS"

# generating random samples using OpenBUGS
model.intr <- bugs( data.names, inits1, model.file = "anova_all_models.txt",
                    parameters = parameter.names,n.chains = 1, 
			  n.iter = 10000, n.burnin=5000, n.thin=1,  
			  bugs.directory = openbugs.dir, debug=F, program="OpenBUGS")



# --------------------------------
# Ergotic mean plots
# -------------------------------- 


par(mfrow=c(4,1))
for (i in 1:4){
x1<-model.intr$sims.matrix[,i]
plot( cumsum(x1)/1:length(x1), type='l',xlab="iterations",
		ylab=paste(colnames(model.intr$sims.matrix)[i] ))
}

par(mfrow=c(4,1))
for (i in 5:8){
x1<-model.intr$sims.matrix[,i]
plot( cumsum(x1)/1:length(x1), type='l',xlab="iterations",
		ylab=paste(colnames(model.intr$sims.matrix)[i] ))
}

par(mfrow=c(4,1))
for (i in 9:12){
x1<-model.intr$sims.matrix[,i]
plot( cumsum(x1)/1:length(x1), type='l',xlab="iterations",
		ylab=paste(colnames(model.intr$sims.matrix)[i] ))
}

par(mfrow=c(3,1))
for (i in 13:15){
x1<-model.intr$sims.matrix[,i]
plot( cumsum(x1)/1:length(x1), type='l',xlab="iterations",
		ylab=paste(colnames(model.intr$sims.matrix)[i] ))
}


# --------------------------------
# Coda menu convergence diagnostics
# --------------------------------

library(coda)
codamenu()





