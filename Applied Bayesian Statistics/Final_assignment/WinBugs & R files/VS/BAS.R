#
#Importing data

library(foreign)
sm<-read.spss("C://Users//part//Desktop//14_supermarkets.sav",use.value.label=TRUE,to.data.frame=TRUE)


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


sm.new<-na.omit(sm)



# Dummies#
#stz parametrization
Dcar <- matrix(0,dim(sm.new)[1],2)
Dsex <- matrix(0,dim(sm.new)[1],2)
Ddistance <- matrix(0,dim(sm.new)[1],4)
DQfresh <- matrix(0,dim(sm.new)[1],5)
DQpack <- matrix(0,dim(sm.new)[1],5)
DQueues <- matrix(0,dim(sm.new)[1],5)
Dprices <- matrix(0,dim(sm.new)[1],5)

for ( i  in 1 : dim(sm.new)[1] ){
             # Dummies for car , sex with 2 levels
             if ( sm.new$car[i]==0 ) { Dcar[i,2] <- -1 }
             if ( sm.new$car[i]==1 ) { Dcar[i,2] <- 1 }
             if ( sm.new$car[i]==0 ) { Dsex[i,2] <- -1 }
             if ( sm.new$car[i]==1 ) { Dsex[i,2] <- 1 }

	# Dummies for distance with 4 levels
	for ( j  in 2 : 4 ){
             if ( sm.new$distance[i]==1 ) { Ddistance[i,j] <- -1 }
             else if ( sm.new$distance[i]==j ) { Ddistance[i,j] <- 1 }
             else { Ddistance[i,j] <- 0 }
	}
	
	# Dummies for prices, Qfresh, Qpack, Queues with 5 levels
	for ( j  in 2 : 5 ){
             if ( sm.new$Qfresh[i]==1 ) { DQfresh[i,j] <- -1 }
             else if ( sm.new$Qfresh[i]==j ) { DQfresh[i,j] <- 1 }
             else { DQfresh[i,j] <- 0 }
             if ( sm.new$Qpack[i]==1 ) { DQpack[i,j] <- -1 }
             else if ( sm.new$Qpack[i]==j ) { DQpack[i,j] <- 1 }
             else { DQpack[i,j] <- 0 }
             if ( sm.new$Queues[i]==1 ) { DQueues[i,j] <- -1 }
             else if ( sm.new$Queues[i]==j ) { DQueues[i,j] <- 1 }
             else { DQueues[i,j] <- 0 }
             if ( sm.new$prices[i]==1 ) { Dprices[i,j] <- -1 }
             else if ( sm.new$prices[i]==j ) { Dprices[i,j] <- 1 }             
             else { Dprices[i,j] <- 0 }
	}
}





sm.new$Dcar <- Dcar[,2]
sm.new$Dsex <- Dsex[,2]
sm.new$Dprices2 <- Dprices[,2]
sm.new$Dprices3 <- Dprices[,3]
sm.new$Dprices4 <- Dprices[,4]
sm.new$Dprices4 <- Dprices[,4]
sm.new$DQfresh2 <- DQfresh[,2]
sm.new$DQfresh3 <- DQfresh[,3]
sm.new$DQfresh4 <- DQfresh[,4]
sm.new$DQfresh5 <- DQfresh[,5]
sm.new$DQpack2 <- DQpack[,2]
sm.new$DQpack3 <- DQpack[,3]
sm.new$DQpack4 <- DQpack[,4]
sm.new$DQpack5 <- DQpack[i,5]
sm.new$DQueues2 <- DQueues[,2]
sm.new$DQueues3 <- DQueues[,3]
sm.new$DQueues4 <- DQueues[,4]
sm.new$DQueues5 <- DQueues[,5]
sm.new$Ddistance2 <- Ddistance[,2]
sm.new$Ddistance3 <- Ddistance[,3]
sm.new$Ddistance4 <- Ddistance[,4]


#sm$spen_yr<-sm$spen_yr/1000

#posotikes ordinal
#sm.n<- sm.new[ ,c(2,4:8,10,11)]
sm.n<- sm.new[ ,c(2,10:dim(sm.new)[2])]

head(sm.n)
View(sm.n)

library(BAS)


#hyper-g beta binomial
res1 <- bas.lm( spen_yr~., data=sm.n, prior='hyper-g', alpha=3 )
round(t(summary(res1, 10)[,-1]),2)   # top 10 montela
res1  # inclusion probabilities

#hyper-g uniform 
res2 <- bas.lm( spen_yr~., data=sm.n, prior='hyper-g', alpha=3 , modelprior=uniform())
round(t(summary(res2, 10)[,-1]),2)   # top 10 montela
res2  # inclusion probabilities


#g-prior beta binomial
res3 <- bas.lm( spen_yr~., data=sm.n, prior='g-prior', alpha=dim(sm.new)[2] )
round(t(summary(res3, 10)[,-1]),2)   # top 10 montela
res3  # inclusion probabilities

#g-prior uniform 
res4 <- bas.lm( spen_yr~., data=sm.n, prior='g-prior', alpha=dim(sm.new)[2] ,modelprior=uniform())
round(t(summary(res4, 10)[,-1]),2)   # top 10 montela
res4  # inclusion probabilities

#BIC beta binomial
res5 <- bas.lm( spen_yr~., data=sm.n, prior='BIC' )
round(t(summary(res5, 10)[,-1]),2)   # top 10 montela
res5  # inclusion probabilities

#BIC uniform 
res6 <- bas.lm( spen_yr~., data=sm.n, prior='BIC', modelprior=uniform())
round(t(summary(res6, 10)[,-1]),2)   # top 10 montela
res6  # inclusion probabilities





#coefficients
coef(res1, estimator='HPM', n.models=100)   # highest probability model (map)
coef(res1, estimator='MPM', n.models=100)   # median probability model 
coef(res2, estimator='HPM', n.models=100)   # highest probability model (map)
coef(res2, estimator='MPM', n.models=100)   # median probability model 
coef(res3, estimator='HPM', n.models=100)   # highest probability model (map)
coef(res3, estimator='MPM', n.models=100)   # median probability model 
coef(res4, estimator='HPM', n.models=100)   # highest probability model (map)
coef(res4, estimator='MPM', n.models=100)   # median probability model 
coef(res5, estimator='HPM', n.models=100)   # highest probability model (map)
coef(res5, estimator='MPM', n.models=100)   # median probability model 
coef(res6, estimator='HPM', n.models=100)   # highest probability model (map)
coef(res6, estimator='MPM', n.models=100)   # median probability model 








#visualisation of important variables
image(res1)
image(res1, top=30)






