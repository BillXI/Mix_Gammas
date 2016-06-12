# case: seperate, equal lambda
## 
set.seed(106)
library("mixtools")
library("MASS")
n.iter=5000
gamma.paras1<- gamma.paras2<- gamma.paras3<- array(NA, dim=c(2,3,n.iter))
gamma.paras1[,,1]<- gamma.paras2[,,1]<- gamma.paras3[,,1]<- matrix(0,nrow=2,ncol=3)
#j=0
i=0
#for( i in 1:n.iter){
while(i < n.iter){
  i <- i+1
  gamma<- c(rgamma(200, shape = 1, scale = 2), 
            rgamma(200, shape = 30, scale = 1),
            rgamma(200, shape = 50, scale = 2))
  normal<- gamma^(1/3)  
  n <- normalmixEM(normal, k=3)
  
  group1=gamma[apply(n$posterior,1,which.max)==1]
  group2=gamma[apply(n$posterior,1,which.max)==2]
  group3=gamma[apply(n$posterior,1,which.max)==3]
  
  
  if(any(length(group1)<=10|length(group2)<=10|length(group3)<=10)==FALSE){
    #  if(any(length(group1)<=10|length(group2)<=10|length(group3)<=10|j>n.iter|!is.na(sum(gamma.paras[,,i])))==FALSE){
    ## here we check if any NAs prodeced, and discard them
    #    j=j+1
    all.groups <- list(group1,group2,group3) #This puts the three groups in a list.
    all.groups <- all.groups[order(sapply(all.groups,mean))] #This reorders the three groups based on their sample means, from smallest to largest.
    names(all.groups) <- sapply(1:length(all.groups),function(i) paste("groups",i,sep=""))
    
    # codes to estimate starting values of alphas and betas for EM algorithm 
    f1 <-glm(group1~1,family=Gamma())
    shape1 <- gamma.shape(f1)
    fit1<- fitdistr(group1,"gamma",start=list(shape=shape1$alpha,rate=coef(f1)*shape1$alpha)) #rate is 1/beta
    
    f2 <- glm(group2~1,family=Gamma())
    shape2 <- gamma.shape(f2) 
    fit2<- fitdistr(group2,"gamma",start=list(shape=shape2$alpha,rate=coef(f2)*shape2$alpha))
    
    f3 <-glm(group3~1,family=Gamma())
    shape3 <- gamma.shape(f3)
    fit3<- fitdistr(group3,"gamma",start=list(shape=shape3$alpha,rate=coef(f3)*shape3$alpha)) 
    
    # EM algorithm for 3 situations
    para1<- gammamixEM(gamma, k=3,      # not specify starting values
                       eps=10^(-5), verb = FALSE)
    
    para2<- gammamixEM(gamma, k=3,        # ideal case/when we know exact parameter values
                       alpha=c(1, 30, 50),     
                       beta=c(2, 1, 2),
                       eps=10^(-5), verb = FALSE)
    
    para3<- gammamixEM(gamma, lambda = n$lambda,  # use normal-gamma estimation scheme 
                       alpha=c(fit1$estimate[1], fit2$estimate[1], fit3$estimate[1]),  
                       beta=c(fit1$estimate[2], fit2$estimate[2], fit3$estimate[2]),
                       eps=10^(-5), verb = FALSE)
    
    new.order1 <- order(apply(para1$gamma.pars,2,prod))# ascending order of means, as consistent with previous step
    new.order2 <- order(apply(para2$gamma.pars,2,prod))
    new.order3 <- order(apply(para3$gamma.pars,2,prod))
    
    para1$lambda <- para1$lambda[new.order1]
    para2$lambda <- para2$lambda[new.order2]
    para3$lambda <- para3$lambda[new.order3]
    
    para1$gamma.pars <- para1$gamma.pars[,new.order1]
    para2$gamma.pars <- para2$gamma.pars[,new.order2]
    para3$gamma.pars <- para3$gamma.pars[,new.order3]
    
    # and we will arrange comp1-comp3 in ascending order
    colnames(para1$gamma.pars) <- sapply(1:ncol(para1$gamma.pars),function(i) paste("comp.",i,sep=""))
    colnames(para2$gamma.pars) <- sapply(1:ncol(para2$gamma.pars),function(i) paste("comp.",i,sep=""))
    colnames(para3$gamma.pars) <- sapply(1:ncol(para3$gamma.pars),function(i) paste("comp.",i,sep=""))
    
    gamma.paras1[,,i]<- para1$gamma.pars
    gamma.paras2[,,i]<- para2$gamma.pars
    gamma.paras3[,,i]<- para3$gamma.pars
    
    if(is.na(sum(gamma.paras1[,,i]))|is.na(sum(gamma.paras2[,,i]))|is.na(sum(gamma.paras3[,,i]))) i <- i-1
  }else i <- i-1
  print(i)
}

# full list of iterations of 3 cases, respectively
gamma.paras1

gamma.paras2

gamma.paras3


### what we need for MSE ( iterations)

# not specify starting values
apply((gamma.paras1-array(data=rep(matrix(c(1,2,30,1,50,2)),n.iter), dim=c(2,3,n.iter)))^2, c(1,2), mean, na.rm=TRUE)

# ideal case where we use the exact starting values
apply((gamma.paras2-array(data=rep(matrix(c(1,2,30,1,50,2)),n.iter), dim=c(2,3,n.iter)))^2, c(1,2), mean, na.rm=TRUE)

# use normal-gamma estimation scheme 
apply((gamma.paras3-array(data=rep(matrix(c(1,2,30,1,50,2)),n.iter), dim=c(2,3,n.iter)))^2, c(1,2), mean, na.rm=TRUE)

# not specify starting values
> apply((gamma.paras1-array(data=rep(matrix(c(1,2,30,1,50,2)),n.iter), dim=c(2,3,n.iter)))^2, c(1,2), mean, na.rm=TRUE)
[,1]         [,2]         [,3]
[1,]  2.087039 3.505506e+13 4.784084e+06
[2,] 60.618397 1.033992e+02 5.958553e+01
> 
  > # ideal case where we use the exact starting values
  > apply((gamma.paras2-array(data=rep(matrix(c(1,2,30,1,50,2)),n.iter), dim=c(2,3,n.iter)))^2, c(1,2), mean, na.rm=TRUE)
[,1]        [,2]       [,3]
[1,] 0.01002535 150.4137199 89.8330543
[2,] 0.32044277   0.5379599  0.1945506
> 
  > # use normal-gamma estimation scheme 
  > apply((gamma.paras3-array(data=rep(matrix(c(1,2,30,1,50,2)),n.iter), dim=c(2,3,n.iter)))^2, c(1,2), mean, na.rm=TRUE)
[,1]         [,2]       [,3]
[1,] 0.02078353 19902.239103 3846.30310
[2,] 0.61529878     3.871946   68.70586
> 
