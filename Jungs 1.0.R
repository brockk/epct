#Jungs error rates calculator for sample size (two stage design)

#n1 - number stage 1 (per arm)
#n2- number stage 2 recruited (per arm) (not the total)
#a1- number of responders 1st stage(difference)
#a - number of total resonders(difference)
#p0 - repsonse rate control arm
#p1 - repsonse rate experimental arm


Jungserr2 <- function(n1, n2, a1, a, p0, p1) {

  alphas<-numeric(0)  
  
  for(i in a1:n1) {
    for(j in max(0,(-i)):(n1-max(0,i))) {
      for(k in (a-i):n2) {
        for(l in max(0,(-k)):(n2-max(0,k))) {
          alpha <- (dbinom(j, size = n1, prob = p0) * dbinom((i + j), size = n1, prob = p0) * dbinom(l, size = n2, prob = p0) * dbinom((k + l), size = n2, prob = p0))
          alphas = c(alphas, alpha)
        }
      }
    }
  }
  

  betas<-numeric(0)  
  
  for(i in a1:n1) {
    for(j in max(0,(-i)):(n1-max(0,i))) {
      for(k in (a-i):n2) {
        for(l in max(0,(-k)):(n2-max(0,k))) {
          beta <- (dbinom(j, size = n1, prob = p0) * dbinom((i + j), size = n1, prob = p1) * dbinom(l, size = n2, prob = p0) * dbinom((k + l), size = n2, prob = p1))
          betas = c(betas, beta)
        }
      }
    }
  }
  
return(c("alpha" = sum(alphas), "power" = (sum(betas))))
}

#Jungs error rates calculator for single stage

#n-number of participants (per arm)
#a-number of responders(difference)
#p0 - repsonse rate control arm
#p1 - repsonse rate experimental arm

Jungserr <- function(n, a, p0, p1) {
  alphas<-numeric(0)  
  for(i in a:n){
    for(j in max(0, (-i)):(n - max(0, i))) {
      alpha <- (dbinom(j, size = n, prob = p0) * dbinom((i + j), size = n, prob = p0))
      alphas = c(alphas, alpha)
    }
  }
  
  betas<-numeric(0)  
  for(i in a:n) {
    for(j in max(0, (-i)):(n - max(0, i))) {
      beta <- (dbinom(j, size = n, prob = p0) * dbinom((i + j), size = n, prob = p1))
      betas = c(betas, beta)
    }
  }
  return(c("alpha" = sum(alphas), "power" = (sum(betas))))
}
















