# Dynamic-Conditional-Correlation-DCC-Model-

########let a be the data with four variables and set it as a time series. Suppose a is monthly data that starts with January 1997.
ac=ts(data=a, start=c(1997,1), frequency=12)

#######Install and Load the following two packages
library(ccgarch)
library(fGarch)

############setting the initial values for the maximum liklihood estimation of the DCC#######
f1 = garchFit(~ garch(1,1), data=ac[,1],include.mean=T, include.delta=T)
f1 = f1 @ fit$coef
f2 = garchFit(~ garch(1,1), data=ac[,2],include.mean=T, include.delta=T)
f2 = f2 @ fit$coef
#f2=predict(f2)
f3 = garchFit(~ garch(1,1), data=ac[,3],include.mean=T, include.delta=T)
f3 = f3 @ fit$coef
#f3=predict(f3)
f4 = garchFit(~ garch(1,1), data=ac[,4],include.mean=T, include.delta=T)
f4 = f4 @ fit$coef

e = c(f1[1], f2[1], f3[1], f4[1])
g = diag(c(f1[2],f2[2],f3[2],f4[2]))
h = diag(c(f1[3], f2[3], f3[3], f4[3]))

########parameter initial values#########
dccpara = c(0.1,0.2)

############DCC estimation###########
dccresults1 = dcc.estimation(inia=e, iniA=g, iniB=h, ini.dcc=dccpara,dvar=ac, model="diagonal", method= "BFGS", gradient=0)

###### extracting the conditional variance of each variable 
z=data.frame(dccresults1$h)

#############extracting the conditional corre;ation amongst the variables##########
j=data.frame(dccresults1$DCC)
