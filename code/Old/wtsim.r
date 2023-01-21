rm(list=ls(all=TRUE))
script.dir<-"C:\\data\\BPA Projects\\BPA CWT\\PIT Tag Columbia Harvest\\Code\\2012-2015\\"

#made up params
sigmalogmu<-0.1
sigmalogsigma<-0.1
periods<-20
wt<-c(NULL)
p<-c(NULL)
mu<-c(NULL)
sigma<-c(NULL)
mu[1]<-15
sigma[1]<-0.3
pmissing<-0.2
#sim data
for (i in 1:periods){
  ss<-runif(1,1,200)
  wt<-c(wt,round(exp(rnorm(ss,log(mu[i]),sigma[i]))))
  p<-c(p,rep(i,ss))
  if(i<periods){
    mu[i+1]<-exp(rnorm(1,log(mu[i]),sigmalogmu))
    sigma[i+1]<-exp(rnorm(1,log(sigma[i]),sigmalogsigma))
  }
}

wdat<-data.frame(cbind(p,wt))
colnames(wdat)<-c("period","weight")
wdat2<-wdat

for(i in 1:periods){
  nodat<-rbinom(1,1,1-pmissing)
  if(nodat==0){wdat2$weight[wdat$period==i]<-NA}
}

means<-as.data.frame(wdat %>% group_by(period)%>%summarise(mean=mean(weight)))
means2<-as.data.frame(wdat2 %>% group_by(period)%>%summarise(mean=mean(weight),ss=length(period)))
trudat<-data.frame(cbind(merge(means2,means,by="period"),mu))

#jagsdata
jagsdata<-list(
  logweight=log(wdat2$weight),
  period=wdat2$period,
  periods=periods,
  N_bio=dim(wdat2)[1]
)

#params
pars<-c("mu_wt","mu_proc_resid","sigma_logmu_wt","sigma_wt", "sigma_log_sigma_wt")

jagsfit <- jags.parallel(jagsdata, 
                         inits=NULL, 
                         model.file=paste(script.dir,"wt.txt",sep=""),
                         n.chains=3, 
                         n.thin=10, 
                         n.burnin=10000, 
                         n.iter=30000,
                         parameters.to.save=pars)
plotdat<-as.matrix(jagsfit$BUGSoutput$sims.list$mu_wt)
bp<-boxplot.matrix(plotdat,outline=F,plot=F)
bp$stats[1,]<-apply(plotdat,2,function(x) quantile(x,0.025))
bp$stats[2,]<-apply(plotdat,2,function(x) quantile(x,0.25))
bp$stats[3,]<-apply(plotdat,2,function(x) quantile(x,0.5))
bp$stats[4,]<-apply(plotdat,2,function(x) quantile(x,0.75))
bp$stats[5,]<-apply(plotdat,2,function(x) quantile(x,0.975))
bp$out<-NULL
bxp(bp,outline=F)
#points(trudat$mean.y~c(1:periods),col="red",pch="-",cex=3)
points(trudat$mean.x~c(1:periods),col="blue",pch="-",cex=3)
points(trudat$mu~c(1:periods),col="blue",pch=20,cex=1)
hist(jagsfit$BUGSoutput$median$mu_wt-mu)
plot(density(jagsfit$BUGSoutput$sims.list$mu_wt[,3]))
#look at data and ests
print(round(jagsfit$BUGSoutput$summary,3))
print(means)
print(means2)
print(mu)
print(wdat2)
