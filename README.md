# Bayes-Music-Prediction
hierarchical bayes mixture model, predicting hits in experimental music

### Project Page

[Markdown Link](https://katjanewilson.github.io/Bayes-Music-Prediction/)

## Analysis Overview

A normal-normal mixture model with EM algorithm, then hierarchical bayes. See Markdown Link

``` {r}

#Expectation function
Estep <- function(y,alpha,mu0,mu1,sigsq0,sigsq1){
  n <- length(y)  
  ind <- rep(NA,n)
  for (i in 1:n){
    prob0 <- (1-alpha)*dnorm(y[i],mean=mu0,sd=sqrt(sigsq0))
    prob1 <- alpha*dnorm(y[i],mean=mu1,sd=sqrt(sigsq1))
    ind[i] <- prob1/(prob0+prob1)
  }
  ind
}

#Maximization function
Mstep <- function(y,ind){
  n <- length(y)
  alpha <- sum(ind)/n
  mu1 <- sum(ind*y)/sum(ind)
  mu0 <- sum((1-ind)*y)/sum(1-ind)
  sigsq1 <- sum(ind*((y-mu1)^2))/sum(ind)
  sigsq0 <- sum((1-ind)*((y-mu0)^2))/sum(1-ind)
  c(alpha,mu0,mu1,sigsq0,sigsq1)
}

##observed data loglikelihood function
loglik.mix <- function(y,ind,alpha,mu0,mu1,sigsq0,sigsq1){
  loglik <- sum(log(alpha*dnorm(y,mu1,sqrt(sigsq1))+(1-alpha)*dnorm(y,mu0,sqrt(sigsq0))))
  loglik
}
#Running EM iterations
curalpha <- 0.5
curmu0 <- 0.1
curmu1 <- 0.4
cursigsq0 <- 0.03
cursigsq1 <- 0.03
mean(mean_pop)
curind <- Estep(mean_pop,curalpha,curmu0,curmu1,cursigsq0,cursigsq1)
loglik <- loglik.mix(mean_pop,curind,curalpha,curmu0,curmu1,cursigsq0,cursigsq1)
itermat <- c(curalpha,curmu0,curmu1,cursigsq0,cursigsq1,loglik)
diff <- 1
numiters <- 1
while (diff > 0.001 || numiters <= 100){
  curind <- Estep(mean_pop,curalpha,curmu0,curmu1,cursigsq0,cursigsq1)
  curparam <- Mstep(mean_pop,curind)
  curalpha <- curparam[1]
  curmu0 <- curparam[2]
  curmu1 <- curparam[3]
  cursigsq0 <- curparam[4]
  cursigsq1 <- curparam[5]
  itermat <- rbind(itermat,c(curparam,loglik))
  loglik <- loglik.mix(mean_pop,curind,curalpha,curmu0,curmu1,cursigsq0,cursigsq1)
  numiters <- numiters + 1
  diff <- max(abs(itermat[numiters,]-itermat[numiters-1,])) 
  # print (c(numiters,loglik))
}

```

# citations
