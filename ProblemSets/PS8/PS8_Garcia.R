library(nloptr)
library(stargazer)

#4. create a data set that has the following properties: set.seed(100), X is a matrix dimension N =  100,000 by K = 10 containing normally distributed random numbers, expect the first column which whould be a column of 1's, epsilon (eps) is a vector length N containing random numbers distributed N(0, sigma^2) where sigma is 0.5 and sigma^2 is 0.25, beta is a vector of length 10 and has the following values [1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2], and generate Y which is equal to XBeta + epsilon

set.seed(100)
N<- 100000
K<- 10
sigma<- 0.5
eps<- rnorm(N,mean=0,sd=0.5)
X<- matrix(rnorm(N*K,mean=0,sd=sigma),N,K) 

X[,1]<- 1
eps <- rnorm(N,mean=0,sd=0.5) 
beta<- c(1.5,-1,-0.25,0.75,3.5,-2,0.5,1,1.25,2)

Y<- X%*%beta + eps
estimates<- lm(Y~X -1)
print(summary(estimates))

#5. Using the matrices you just generated, compute BetaOLS, which is the OLS estimate of beta using the closed-form solution. How does your estimate compare with the true value of beta in 1

betaOLS<- solve(crossprod(X))%*%crossprod(X,Y)
betaOLS
#My estimates are pretty close to the true values we assigned to beta in the previous question. 

#6. Compute BetaOLS using gradient descent (as we went over in class). Make sure you appropriately code the gradient vector. Set the learning rate (step size) to equal 0.0000003.

# set up a stepsize
alpha<- 0.0000003
# set up a number of iterations
maxiter<- 500000
# our objective function
objfun<- function(beta,Y,X) {
return (sum((Y-X%*%beta)^2))  
}
# define the gradient of our objective function
gradient<- function(beta,y,X){
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)))
}
# intial values
beta<- runif(dim(X)[2])
# randomly intialize a value to beta
set.seed(100)
# create a vector to contain al beta's for all steps
beta.All<- matrix("numeric",length(beta),maxiter)
# gradient descent method to find the minimum
iter<- 1
beta0<- 0*beta
while(norm(as.matrix(beta0)-as.matrix(beta))>1e-8){
  beta0<- beta
  beta<- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter]<- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter<- iter+1
}
# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,Y,X) is ",beta,sep = ""))

#7. Compute BetaOLS using nloptr's L-BFGS algorithm. Do it again using the Nelder-Mead algorithm. Do your answers differ? [i am having issues with this one]

## LBFGS algortih
# objective function
objfun<- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
}
# gradient of our objective function
gradient<- function(beta,Y,X) {
  return(as.vector(-2*t(X)%*%(Y-X%*%beta)))
}
# intial values
Beta0<- runif(dim(X)[2])
# algorithm parameters
options<- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# optimize
result<- nloptr(x0=Beta0,eval_f = objfun,eval_grad_f = gradient, opts = options, Y=Y, X=X)
print(result)

## Nelder-Mead algorithm
# objective function
objfun<- function(theta,Y,X) {
  beta<- beta[1:(length(beta)-1)]
  sig <- beta[length(beta)]
  loglike<- -sum(-.5*(log(2*pi*(sig^2)) + ((Y-X%*%theta)/sig)^2))
  return(loglike)
}
# initial values 
theta0<- runif(dim(X)[2]+1)
# algorithm parameters
options<- list("algortithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
# optimize
result<- nloptr(x0=theta0,eval_f = objfun,opts = options,Y=Y,X=X)

#8. Now compute BetaMLE using nloptrs LBFGS algorithm. 
gradient<- function(theta,Y,X){
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig  <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%8%(Y-X%8%beta)/(sig^2)
  grad[length(theta)]       <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/(sig^3)
  return(grad)
}
# intial values
theta0<- runif(dim(X)[2]+1)
# algorithm parameters
options<- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
# optimize
result<- nloptr(x0=theta0,eval_f = objfun,opts = options,Y=Y,X=X)
print(result)

#9. Now compute BetaOLS the easy way. Use Stargazer to export the regression.
linear<- lm(Y ~ X - 1)
stargazer(linear)
