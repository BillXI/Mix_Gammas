# change convergence criterion to 10^-3, and run 2500 iterations, and make the overlap case "separate" a bit so that it speeds up a bit


# case: overlap, equal lambda
## 
install.packages("mixtools")
install.packages("MASS")

set.seed(123)
library("mixtools")
library("MASS")
n.iter=260
gamma.paras1oez<- gamma.paras2oez<-gamma.paras3oez<- array(NA, dim=c(2,2,n.iter))
gamma.paras1oez[,,1]<- gamma.paras2oez[,,1]<-gamma.paras3oez[,,1]<- matrix(0,nrow=2,ncol=2)
lambda1<-lambda2<-lambda3<-matrix(NA, ncol=n.iter, nrow=2)
loglik1<-loglik2<-loglik3<-rep(NA, n.iter)

#
gamma.list=matrix(NA, ncol=n.iter, nrow=400)
#j=0
i=0
n.iter=260
#for( i in 1:n.iter){
while(i < n.iter){
  i <- i+1
  gamma<- c(rgamma(200, shape = 5, scale = 5), 
            rgamma(200, shape = 2, scale = 3))
  gamma.list[,i]=gamma

  normal<- gamma^(1/3)  
  n <- normalmixEM(normal, k=2)
  
  group1=gamma[apply(n$posterior,1,which.max)==1]
  group2=gamma[apply(n$posterior,1,which.max)==2]
  
  if(any(length(group1)<=10|length(group2)<=10)==FALSE){
    #  if(any(length(group1)<=10|length(group2)<=10|length(group3)<=10|j>n.iter|!is.na(sum(gamma.paras[,,i])))==FALSE){
    ## here we check if any NAs prodeced, and discard them
    #    j=j+1
    all.groups <- list(group1,group2) #This puts the three groups in a list.
    all.groups <- all.groups[order(sapply(all.groups,mean))] #This reorders the three groups based on their sample means, from smallest to largest.
    names(all.groups) <- sapply(1:length(all.groups),function(i) paste("groups",i,sep=""))
    
    # codes to estimate starting values of alphas and betas for EM algorithm 
    f1 <-glm(group1~1,family=Gamma())
    shape1 <- gamma.shape(f1)
    fit1<- fitdistr(group1,"gamma",start=list(shape=shape1$alpha,rate=coef(f1)*shape1$alpha)) #rate is 1/beta
    
    f2 <- glm(group2~1,family=Gamma())
    shape2 <- gamma.shape(f2) 
    fit2<- fitdistr(group2,"gamma",start=list(shape=shape2$alpha,rate=coef(f2)*shape2$alpha))
    
    # EM algorithm for 3 situations
    para1<- gammamixEM(gamma, k=2,      # not specify starting values
                       eps=10^(-3), verb = FALSE)
    
    para2<- gammamixEM(gamma, k=2,        # ideal case/when we know exact parameter values
                       alpha=c(2, 5),     
                       beta=c(3, 5),
                       eps=10^(-3), verb = FALSE)
    
    para3<- gammamixEM(gamma, lambda = n$lambda,  # use normal-gamma estimation scheme 
                       alpha=c(fit1$estimate[1], fit2$estimate[1]),  
                       beta=c(fit1$estimate[2], fit2$estimate[2]),
                       eps=10^(-3), verb = FALSE)
    
 # not need to change from the k=3 case
    new.order1 <- order(apply(para1$gamma.pars,2,prod))# ascending order of means, as consistent with previous step
    new.order2 <- order(apply(para2$gamma.pars,2,prod))
    new.order3 <- order(apply(para3$gamma.pars,2,prod))
    
    para1$lambda <- para1$lambda[new.order1]
    para2$lambda <- para2$lambda[new.order2]
    para3$lambda <- para3$lambda[new.order3]
 
    lambda1[,i]<-para1$lambda
    lambda2[,i]<-para2$lambda
    lambda3[,i]<-para3$lambda
 
    para1$gamma.pars <- para1$gamma.pars[,new.order1]
    para2$gamma.pars <- para2$gamma.pars[,new.order2]
    para3$gamma.pars <- para3$gamma.pars[,new.order3]
    
    # and we will arrange comp1-comp3 in ascending order
    colnames(para1$gamma.pars) <- sapply(1:ncol(para1$gamma.pars),function(i) paste("comp.",i,sep=""))
    colnames(para2$gamma.pars) <- sapply(1:ncol(para2$gamma.pars),function(i) paste("comp.",i,sep=""))
    colnames(para3$gamma.pars) <- sapply(1:ncol(para3$gamma.pars),function(i) paste("comp.",i,sep=""))
 
    loglik1[i]<-para1$loglik
    loglik2[i]<-para2$loglik
    loglik3[i]<-para3$loglik
 
    gamma.paras1oez[,,i]<- para1$gamma.pars
    gamma.paras2oez[,,i]<- para2$gamma.pars
    gamma.paras3oez[,,i]<- para3$gamma.pars
    
    if(is.na(sum(gamma.paras1oez[,,i]))|is.na(sum(gamma.paras2oez[,,i]))|is.na(sum(gamma.paras3oez[,,i]))) i <- i-1
  }else i <- i-1
  print(i)
}



# below are the compilation of parameters we might be interested in:

# full list of generated gamma from iteration to iteration
gamma.list

# full list of output iterations of 3 cases, respectively
gamma.paras1oez

gamma.paras2oez

gamma.paras3oez

# full list of lambdas, of 3 different methods
lambda1
lambda2
lambda3

# mse for lambdas, of 3 different methods
mse.lambda1<-apply((lambda1-matrix(c(0.5), ncol=n.iter, nrow=2))^2, 1, mean)  # mse for lambda1
mse.lambda1

mse.lambda2<-apply((lambda2-matrix(c(0.5), ncol=n.iter, nrow=2))^2, 1, mean)  # mse for lambda2
mse.lambda2

mse.lambda3<-apply((lambda3-matrix(c(0.5), ncol=n.iter, nrow=2))^2, 1, mean)  # mse for lambda3
mse.lambda3

# log-likehoods, of 3 different methods
loglik1
loglik2
loglik3

### final MSE table, cbind with lambdas

# not specify starting values
mse1<-apply((gamma.paras1oez-array(data=rep(matrix(c(2,3,5,5)),n.iter), dim=c(2,2,n.iter)))^2, c(1,2), mean, na.rm=TRUE)
cbind(mse1, mse.lambda1)

# ideal case where we use the exact starting values
mse2<-apply((gamma.paras2oez-array(data=rep(matrix(c(2,3,5,5),),n.iter), dim=c(2,2,n.iter)))^2, c(1,2), mean, na.rm=TRUE)
cbind(mse2, mse.lambda2)

# use normal-gamma estimation scheme 
mse3<-apply((gamma.paras3oez-array(data=rep(matrix(c(2,3,5,5)),n.iter), dim=c(2,2,n.iter)))^2, c(1,2), mean, na.rm=TRUE)
cbind(mse3, mse.lambda3)

g256=gamma.list[,256]
g257=gamma.list[,257]

gamma

