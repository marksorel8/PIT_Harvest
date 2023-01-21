model{
  for ( i in 1:N_bio){ 
    logweight[i] ~ dnorm(logmu_wt[period[i]],tau_wt[period[i]])
    #logweight[i] ~ dnorm(logmu_wt[period[i]],tau_wt)
  }
  for( j in 2: periods) {
    mu_wt[j]<-exp(logmu_wt[j])
    logmu_wt[j] ~ dnorm(logmu_wt[j-1],tau_logmu_wt)
    tau_wt[j]<-pow(sigma_wt[j],-2)
    sigma_wt[j]<-exp(log_sigma_wt[j])
    log_sigma_wt[j] ~ dnorm(log_sigma_wt[j-1],tau_log_sigma_wt)
  }
  for( j in 1:periods) {
    log_T_lbs[j] ~ dnorm(log_total_mu[j], log_total_tau[j])
    log_total_mu[j]<-logmu_wt[j]+log_T_catch[j]
    log_total_tau[j]<-1/((1/tau_wt[j])+log_T_catch[j])
    T_lbs[j]<-exp(log_T_lbs[j])
    log_T_catch[j]<-log(T_catch[j])
    T_catch[j]~dpois(lambda_catch[j])
    lambda_catch[j]~dgamma(1E-06,1E-06)
  }
  #prior for among period weight mean and variance
  mu_wt[1]<-exp(logmu_wt[1])
  logmu_wt[1] ~ dnorm(0,0.1) I(,4) 
  tau_logmu_wt<-pow(sigma_logmu_wt,-2)
  sigma_logmu_wt ~ dnorm(0,1) T(0,)
  #tau_wt<-pow(sigma_wt,-2)
  #sigma_wt ~ dunif(0,10)
  tau_wt[1]<-pow(sigma_wt[1],-2)
  sigma_wt[1]<-exp(log_sigma_wt[1])
  log_sigma_wt[1]~dnorm(-2.78,.1) 
  tau_log_sigma_wt<-pow(sigma_log_sigma_wt,-2)
  sigma_log_sigma_wt ~ dnorm(0,1) T(0,)
}