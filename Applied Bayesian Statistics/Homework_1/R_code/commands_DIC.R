

#dic model1.exp
0.5*var(model1.exp$sims.matrix[,"deviance"])+mean(model1.exp$sims.matrix[,"deviance"])

#dic model2.gamma
0.5*var(model2.gamma$sims.matrix[,"deviance"])+mean(model2.gamma$sims.matrix[,"deviance"])

#dic model3.lnorm
0.5*var(model3.lnorm$sims.matrix[,"deviance"])+mean(model3.lnorm$sims.matrix[,"deviance"])

