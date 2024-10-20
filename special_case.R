#Method of thinning in special case
lambda <- function(time){
  ifelse(time<11 & time>10,100,0.1)
  
}

t_min <- 0
t_max <- 20

# Evaluate lambda on regular grid for plotting
t <- seq(t_min, t_max, length.out = 1001)
lambda_vals <- lambda(t)

lambda_max <- 100
n_homo <- rpois(n = 1, lambda = lambda_max * (t_max - t_min))
x_homo <- runif(n = n_homo, min = t_min, max = t_max)

# Thin point pattern
prob_keep <- lambda(x_homo) / lambda_max
is_kept <- rbinom(n = n_homo, size = 1, prob = prob_keep)

x_inhomo <- x_homo[is_kept == 1]
n_inhomo <- length(x_inhomo)
plot(
  x = t, 
  y = lambda_vals,
  type = "l", 
  lwd = 2,
  ylim = c(0,100),
  ylab = expression(lambda(t)), 
  bty = "n",
  main='Thinning method') 
points(
  x = x_inhomo,
  y = rep(0, n_inhomo),
  pch = 16,
  col = rgb(0,0,0,0.2))







#Method by interval in special case
lambda <- function(time){
  ifelse(time<11 & time>10,100,0.1)
  
}
simulate_ihpp_by_intervals <- function(t_min, t_max){
  event_times <- c()
  t_next <- t_min + rexp(n = 1, rate = lambda(t_min))
  print(t_next)
  while(t_next <= t_max){
    event_times <- c(event_times, t_next)
    l=lambda(t_next)
    t_next <- t_next + rexp(n = 1, rate = l)
    print (t_next)
  }
  
  return(event_times)
}
t_max=20
t_min=0
ihpp_1 <- simulate_ihpp_by_intervals(t_min, t_max) 


t <- seq(t_min, t_max, length.out = 1001)
lambda_vals <- lambda(t)

plot(
  x = t,
  y = lambda_vals,
  type = "l",
  lwd = 2,
  ylim = c(0, lambda(10.5)),
  ylab = expression(lambda(t)), 
  bty='n',
  main='Irritation method')
points(
  x = ihpp_1,
  y = rep(0, length(ihpp_1)),
  pch = 16, 
  xlim = c(0, t_max),
  ylim = c(0, 120),
  
  col = rgb(0, 0, 0, 0.2)) 


