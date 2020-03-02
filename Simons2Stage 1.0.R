#Simons Two-stage Error Rates for given designs


#n1- stage 1 sample size
#r1 - stage 1 critical value (minimum repsonses required)
#n2 - stage 2 sample size (extra patients recruited)
#r - final critical value (minimum repsonses required)
#n - final sample size 
#p0 - undesirable repsonse rate
#p1 - desirable response rate

Simons2Stage<-function(n1,r1,n,r,p0,p1,n2){
Alphas<-numeric(0);  
for(i in (r1):n1){
A<-numeric(0);
r2<-numeric(0);
if((i >= (r1)) & (i<(r))){r2<-((r)-i)} else{r2<-0}
for(j in r2:n2){a<-choose(n2,j)*((p0)^(j))*((1-p0)^(n2-j))
A<-c(A,a)
}
A<-sum(A)
alpha<-((dbinom(i,size=n1,prob = p0))*A) 
Alphas<-c(Alphas,alpha)}

Betas<-numeric(0);  
for(i in (r1):n1){
  B<-numeric(0);
  r2<-numeric(0);
  if((i >= (r1)) & (i<(r))){r2<-((r)-i)} else{r2<-0}
  for(j in r2:n2){b<-choose(n2,j)*((p1)^(j))*((1-p1)^(n2-j))
  B<-c(B,b)
  }
  B<-sum(B)
  beta<-((dbinom(i,size=n1,prob = p1))*B) 
  Betas<-c(Betas,beta)}

return(c("alpha"=sum(Alphas),"power"=sum(Betas)))}









