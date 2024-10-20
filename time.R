#sine
starttime<-Sys.time()
lambda <- function(theta,time){
  
  theta[1]*sin(time)+theta[2]#if here is +1.5, function achieve 0 sometimes and fewer points are generated.
  
}
theta_true=c(40,50)
simulate_ihpp_by_intervals <- function(t_min, t_max){
  event_times <- c()
  t_next <- t_min + rexp(n = 1, rate = lambda(theta_true,t_min))
  print(t_next)
  while(t_next <= t_max){
    event_times <- c(event_times, t_next)
    l=lambda(theta_true,t_next)
    t_next <- t_next + rexp(n = 1, rate = l)
    print (t_next)
  }
  
  return(event_times)
}

ihpp_1 <- simulate_ihpp_by_intervals(t_min = pi, t_max = 5*pi) 

'plot(
  x = ihpp_1,
  y = rep(0, length(ihpp_1)),
  pch = 16,
  xlim = c(pi, 5*pi),
  ylim = c(0, 100),
  xlab = "time, t",
  ylab = expression(lambda(theta_true,t)), 
  bty = "n", 
  col = rgb(0,0,0,0.2)
  
)

lines(
  x <- c(seq(pi,5*pi,length.out=1001)),
  y <- c(lambda(theta_true,x))
  
  
)'

Lambda <- function(theta,time){
  theta[1]* (1 - cos(time)) + theta[2] * time 
}
t_min<-pi
t_max<-5*pi
t <- seq(t_min, t_max, length.out = 1001)
lambda_vals <- lambda(theta = theta_true, time = t)

nllh_ihpp <- function(theta,x){
  
  # !! Need intensity non-negative everywhere
  if (theta[1] > theta[2]) return(1e8)
  # Integral term 
  Lambda_term <- Lambda(theta,5*pi) - Lambda(theta,pi)
  
  # Intensity term
  lambda_term <- -sum(log(lambda(theta,x))) 
  
  return(lambda_term + Lambda_term)
}

opt <- optim(
  par=c(0,5),
  fn = nllh_ihpp,           # function we want to optimise
  method = "L-BFGS-B",      # A smarter version of gradient descent
  hessian = TRUE,           # Return numerical estimate of Hessian at MLE
  x = ihpp_1
  
)
opt

fitted_intensity <- lambda(theta=opt$par, time=t)

plot(
  x = t, 
  y = lambda(theta_true,t),
  type = "l", 
  lwd = 2,
  ylim = c(0,100),
  bty = "n") 

points(
  x = ihpp_1,
  y = rep(0, length(ihpp_1)),
  pch = 16,
  xlim = c(pi, 5*pi),
  ylim = c(0, 100),
  xlab = "time, t",
  ylab = expression(lambda(theta_true,t)), 
  bty = "n", 
  col = rgb(0,0,0,0.2)
)

lines(
  x = t, 
  y = fitted_intensity,
  col = "#E87800",
  type = "l",
  lwd = 2,
  ylim = c(0,10))
endtime<-Sys.time()
timediff1=endtime-starttime


statrtime<-Sys.time()
#sine
lambda <- function(theta, time){
  theta[1] * sin(time) + theta[2]
}

Lambda <- function(theta, time){
  theta[1] * (1 - cos(time)) + theta[2] * time 
}

t_min <- pi
t_max <- 5 * pi 
theta_true = c(40, 50)
t <- seq(t_min, t_max, length.out = 1001)
lambda_vals <- lambda(theta = theta_true, time = t)

# Simulate a point pattern by thinning / rejection sampling

# Simulate from bounding HPP
lambda_max <- 90
n_homo <- rpois(n = 1, lambda = lambda_max * (t_max - t_min))
x_homo <- runif(n = n_homo, min = t_min, max = t_max)

# Thin point pattern
prob_keep <- lambda(theta = theta_true, time = x_homo) / lambda_max
is_kept <- rbinom(n = n_homo, size = 1, prob = prob_keep)

x_inhomo <- x_homo[is_kept == 1]
n_inhomo <- length(x_inhomo)

plot(
  x = t, 
  y = lambda_vals,
  type = "l", 
  lwd = 2,
  ylim = c(0,100),
  bty = "n") 
points(
  x = x_inhomo,
  y = rep(0, n_inhomo),
  pch = 16,
  col = rgb(0,0,0,0.2))
# Negative log-likelihood 

nllh_ihpp <- function(theta, x, A){
  # !! Need intensity non-negative everywhere
  if (theta[1] > theta[2]) return(1e8)
  
  # Integral term 
  Lambda_term <- Lambda(theta, A[2]) - Lambda(theta, A[1])
  
  # Intensity term
  lambda_term <- -sum(log(lambda(theta, time = x))) 
  
  return(lambda_term + Lambda_term)
}

# Test: evaluate for lambda = 0 * sin(t) + 5
nllh_ihpp( theta = c(0, 5), x = x_inhomo, A = c(pi, 5*pi))
opt <- optim(
  par = c(0,5),             # starting parameter values
  fn = nllh_ihpp,           # function we want to optimise
  method = "L-BFGS-B",      # A smarter version of gradient descent
  hessian = TRUE,           # Return numerical estimate of Hessian at MLE
  A = c(t_min, t_max),      # Remaining parameters of lambda(t;theta)
  x = x_inhomo
)


fitted_intensity <- lambda(theta = opt$par, time = t)

plot(
  x = t,
  y = lambda_vals,
  type = "l",
  lwd = 2,
  ylim = c(0, 100)) 
points(
  x = x_inhomo,
  y = rep(0, n_inhomo),
  pch = 16, 
  col = rgb(0, 0, 0, 0.2)) 
lines(
  x = t, 
  y = fitted_intensity,
  col = "#E87800",
  type = "l",
  lwd = 2,
  ylim = c(0,10))
endtime<-Sys.time()
timediff2<-endtime-starttime
print(timediff1)
print(timediff2)
