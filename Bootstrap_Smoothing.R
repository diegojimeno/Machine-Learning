#Resampling is one of the most important terms in ML, it consists on sampling from the training data set to
#validate our hypotesis (chosen model), here, I am using Bootstrapping to calculate the variability of alpha statistic
#which is actually the proportion of investment on one asset and hence reduce its variablity around its mean.

#The differences between bootstrapping and cross validation are significant but the most important one is actually the sampling methodology
#which will be exposed in other files.

#Get the data
spxibm <- as.matrix(read.table(
  "http://www.burns-stat.com/pages/Tutor/spx_ibm.txt",
  header=TRUE, sep='\t', row.names=1))

#Factorizes the data into two matrices
spxret <- spxibm[, 'spx']
ibmret <- spxibm[,'ibm']

#spx.boot.sum is the statistic to approximate with bootstrap
spx.boot.sum <- numeric(1000) 
for(i in 1:1000) {
  this.samp <- spxret[ sample(251, 251, replace=TRUE) ]
  spx.boot.sum[i] <- sum(this.samp)
}

plot(density(spx.boot.sum), lwd=3, col="steelblue")
abline(v=sum(spxret), lwd=3, col='gold')


#Bootstrapping smooth functions (moving average)
spx.varsmu <- array(NA, c(251, 20)) # make 251 by 20 matrix
for(i in 1:20) {
  this.samp <- spxret[ sample(251, 251, replace=TRUE) ]
  spx.varsmu[,i] <- supsmu(1:251, (this.samp - mean(this.samp))^2)$y
}
plot(supsmu(1:251, (spxret-mean(spxret))^2), type='l',
     xlab='Days', ylab='Variance')
matlines(1:251, spx.varsmu, lty=2, col='red')

#Beta for IBM stock
coef(lm(ibmret ~ spxret))

#We're bootstrapping the beta of the ibm stock to get a
#sense of its variability
beta.obs.boot <- numeric(1000)
for(i in 1:1000) {
  this.ind <- sample(251, 251, replace=TRUE)
  beta.obs.boot[i] <- coef(lm(
    ibmret[this.ind] ~ spxret[this.ind]))[2]
}

plot(density(beta.obs.boot), lwd=3, col="steelblue")
abline(v=coef(lm(ibmret ~ spxret))[2], lwd=3, col='gold')
