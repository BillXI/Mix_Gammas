# This is a function created to find MLE for the parameters in the gamma distribution
# need four inputs : data(x), tolerance(eps), initial values of two parameters, (alpha, lambda)
nr.gamma <- function(x,eps,alpha,lambda)
{
        n = length(x);
        sumx = sum(x);
        sumlogx = sum(log(x))
        diff = 1;
        h = 0.0000001;
        theta = c(alpha,lambda)
        while(diff>eps)  # keep running this program until a value of diff is smaller than or equal to eps
        {
                theta.old = theta
                g = gamma(alpha) # calculate a gamma function at alpha
                dg = (gamma(alpha+h)-gamma(alpha))/h # calculate the first derivative of gamma function
                d2g = (gamma(alpha+2*h)-2*gamma(alpha+h)+gamma(alpha))/h^2  # calculate the second derivative of gamma function
                s = c(n*log(lambda)+sumlogx-n*dg/gamma(alpha), n*alpha/lambda-sumx)  # score function
                Jbar = matrix(c(n*(d2g*g-(dg^2))/(g^2),-n/lambda,-n/lambda,n*alpha/(lambda^2)),ncol=2)  # observed information
                theta = theta + solve(Jbar,s)  # solve(A,B) : inverse of A times s
                alpha = theta[1];
                lambda = theta[2];
                loglik = n*alpha*log(lambda) + (alpha-1)*sum(log(x)) - lambda*sum(x) - n*log(gamma(alpha)); # evaluate the loglikelihood
                diff = sum(abs(theta-theta.old))  # calculate the difference between old theta and new theta
                print(c(theta,loglik)) # print theta and corresponding loglikelihood at every iteration
        }
        list(theta=theta,cov=solve(Jbar),loglik=loglik) # save the outputs: final estimates for theta and maximized log likelihood
}