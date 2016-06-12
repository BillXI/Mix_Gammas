gammamix.init <- function (x, lambda = NULL, alpha = NULL, beta = NULL, k = 2) 
{
    n <- length(x)
    if (is.null(lambda)) {
    		cond = TRUE
    		while(cond){
        		lambda = runif(k)
        		lambda = lambda/sum(lambda)
        		if(min(lambda)<0.05) cond=TRUE
        }
    }
    else k = length(lambda)
    if (k == 1) {
        x.bar = mean(x)
        x2.bar = mean(x^2)
    }
    else {
        x.sort = sort(x)
        ind = floor(n * cumsum(lambda))
        x.part = list()
        x.part[[1]] = x.sort[1:(ind[1] + 1)]
        for (j in 2:k) {
            x.part[[j]] = x.sort[ind[j - 1]:ind[j]]
        }
        x.bar = sapply(x.part, mean)
        x2.bar = sapply(lapply(x.part, "^", 2), mean)
    }
    if (is.null(alpha)) {
        alpha = x.bar^2/(x2.bar - x.bar^2)
    }
    if (is.null(beta)) {
        beta = (x2.bar - x.bar^2)/x.bar
    }
    list(lambda = lambda, alpha = alpha, beta = beta, k = k)
}



gammamixEM2 <- function (x, lambda = NULL, alpha = NULL, fix.alpha = FALSE, beta = NULL, k = 2, 
    epsilon = 1e-08, maxit = 1000, maxrestarts = 20, verb = FALSE) 
{
if(length(alpha)<k & fix.alpha) 
	stop(paste("Must specify alpha if fix.alpha is TRUE.", 
                  "\n"))
    x <- as.vector(x)
    tmp <- gammamix.init(x = x, lambda = lambda, alpha = alpha, 
        beta = beta, k = k)
    lambda <- tmp$lambda
    alpha <- tmp$alpha
    beta <- tmp$beta
    if(fix.alpha) theta <- beta else theta <- c(alpha, beta)
    k <- tmp$k
    iter <- 0
    mr <- 0
    diff <- epsilon + 1
    n <- length(x)
    dens <- NULL
if(fix.alpha){
    dens <- function(lambda, theta, k, alpha) {
        temp <- NULL
        beta = theta
        for (j in 1:k) {
            temp = cbind(temp, dgamma(x, shape = alpha[j], scale = beta[j]))
        }
        temp = t(lambda * t(temp))
        temp
    }
    old.obs.ll <- sum(log(apply(dens(lambda, theta, k, alpha), 1, sum)))
    ll <- old.obs.ll
    gamma.ll <- function(theta, z, lambda, k, alpha) -sum(z * log(dens(lambda, 
        theta, k, alpha)))
    while (diff > epsilon && iter < maxit) {
        dens1 = dens(lambda, theta, k, alpha)
        z = dens1/apply(dens1, 1, sum)
        lambda.hat = apply(z, 2, mean)
        out = try(suppressWarnings(nlm(gamma.ll, p = theta, lambda = lambda.hat, 
            k = k, z = z, alpha=alpha)), silent = TRUE)
        if (class(out) == "try-error" | any(is.na(lambda.hat))) {
            cat("Note: Choosing new starting values.", "\n")
            if (mr == maxrestarts) 
                stop(paste("Try different number of components?", 
                  "\n"))
            mr <- mr + 1
			tmp <- gammamix.init(x = x, k = k, alpha = alpha)
            lambda <- tmp$lambda
            beta <- tmp$beta
            theta <- beta
            k <- tmp$k
            iter <- 0
            diff <- epsilon + 1
            old.obs.ll <- sum(log(apply(dens(lambda, theta, k, alpha), 
                1, sum)))
            ll <- old.obs.ll
        }
        else {
            beta.hat = out$estimate
            new.obs.ll <- sum(log(apply(dens(lambda.hat, beta.hat, 
                k, alpha), 1, sum)))
            diff <- new.obs.ll - old.obs.ll
            old.obs.ll <- new.obs.ll
            ll <- c(ll, old.obs.ll)
            lambda = lambda.hat
            beta = beta.hat
			theta = beta
            iter = iter + 1
            if (verb) {
                cat("iteration =", iter, " log-lik diff =", diff, 
                  " log-lik =", new.obs.ll, "\n")
            }
        }
    }
} else{
    dens <- function(lambda, theta, k) {
        temp <- NULL
        alpha = theta[1:k]
        beta = theta[(k + 1):(2 * k)]
        for (j in 1:k) {
            temp = cbind(temp, dgamma(x, shape = alpha[j], scale = beta[j]))
        }
        temp = t(lambda * t(temp))
        temp
    }
    old.obs.ll <- sum(log(apply(dens(lambda, theta, k), 1, sum)))
    ll <- old.obs.ll
    gamma.ll <- function(theta, z, lambda, k) -sum(z * log(dens(lambda, 
        theta, k)))
    while (diff > epsilon && iter < maxit) {
        dens1 = dens(lambda, theta, k)
        z = dens1/apply(dens1, 1, sum)
        lambda.hat = apply(z, 2, mean)
        out = try(suppressWarnings(nlm(gamma.ll, p = theta, lambda = lambda.hat, 
            k = k, z = z)), silent = TRUE)
        if (class(out) == "try-error") {
            cat("Note: Choosing new starting values.", "\n")
            if (mr == maxrestarts) 
                stop(paste("Try different number of components?", 
                  "\n"))
            mr <- mr + 1
			tmp <- gammamix.init(x = x, k = k)
            lambda <- tmp$lambda
            alpha <- tmp$alpha
            beta <- tmp$beta
            theta <- c(alpha, beta)
            k <- tmp$k
            iter <- 0
            diff <- epsilon + 1
            old.obs.ll <- sum(log(apply(dens(lambda, theta, k), 
                1, sum)))
            ll <- old.obs.ll
        }
        else {
            theta.hat = out$estimate
            alpha.hat = theta.hat[1:k]
            beta.hat = theta.hat[(k + 1):(2 * k)]
            new.obs.ll <- sum(log(apply(dens(lambda.hat, theta.hat, 
                k), 1, sum)))
            diff <- new.obs.ll - old.obs.ll
            old.obs.ll <- new.obs.ll
            ll <- c(ll, old.obs.ll)
            lambda = lambda.hat
            theta = theta.hat
            alpha = alpha.hat
            beta = beta.hat
            iter = iter + 1
            if (verb) {
                cat("iteration =", iter, " log-lik diff =", diff, 
                  " log-lik =", new.obs.ll, "\n")
            }
        }
    }
}
    if (iter == maxit) {
        cat("WARNING! NOT CONVERGENT!", "\n")
    }
    cat("number of iterations=", iter, "\n")
    theta = rbind(alpha, beta)
    rownames(theta) = c("alpha", "beta")
    colnames(theta) = c(paste("comp", ".", 1:k, sep = ""))
    a = list(x = x, lambda = lambda, gamma.pars = theta, loglik = new.obs.ll, 
        posterior = z, all.loglik = ll, ft = "gammamixEM")
    class(a) = "mixEM"
    a
}



####TEST CODE
set.seed(100)
x <- c(rgamma(200, shape = 0.2, scale = 14), rgamma(200, 
     shape = 32, scale = 10), rgamma(200, shape = 5, scale = 6))
out <- gammamixEM2(x, lambda = c(1, 1, 1)/3, alpha=c(0.2,32,5), fix.alpha=T, verb = TRUE)
out[2:4]

