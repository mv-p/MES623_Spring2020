##Lab 4 -- Optimization Script 

simple.function <- function(var.vec) {
  x <- var.vec[1] # Tells R that x_1 is the first element of var.vec
  y <- var.vec[2] # Tells R that x_2 is the second element of var.vec
  100 * (y - x^2)^2 + (1 - x)^2
}

var.vec <- c(.5,.5) # var.vec contains both values of {x,y}
z <- simple.function(var.vec) # Evaluate the function at var.vec
z

##Unconstrained Optimization 

OO <- optim( # Store optimization output in a list called OO
  par = c(0,0), # par = the initial values for parameters to be optimized over 
  fn = simple.function, # fn = function to be minimized (or maximized) over, should return a scalar
  upper = Inf, # Upper boundary for the decision variables
  lower = -Inf, # Lower boundary for the decision variables
  method = "L-BFGS-B") # Optimization algorithm that allows for interval specification
OO 

##check that it makes sense 

var.vec <- OO$par

y <- simple.function(var.vec)

y

##Constrained Optimization 

rm(list = ls())

set.seed(4200)

N <- 10 #ten lots

hp <- sample(0:2,N,replace=T) # Randomly pick habitat potential for each lot 
c <- sample(1:10,N,replace=T) # Randomly pick cost of each lot 
B <- 12 # Budget is 12 million
THP <- sum(hp) # Total habitat potential when all lots are purchased.
alpha <- 1/(THP)^.5 # Scaling factor so the probability adds to one.

surv.prob <-function(F0){
  PB <- -alpha*(F0%*%hp)^.5 # Probability of survival multiplied by minus one b/c default is to minimize and we want to maximize 
  as.numeric(PB)                      # we use %*% for matrix multiplication 
}

total.cost <- function(F0){ # Function that calculates the total cost, it ONLY takes F as a parameter
  TC <- F0%*%c # Estimate total cost
  as.numeric(TC)}

install.packages("Rsolnp")
library(Rsolnp)

F0 <- rep(1e-3,N) # Create a set of starting values and make them slightly greater than zero.
OO <- solnp(F0, # Starting values
            fun = surv.prob, # Function to minimize
            ineqfun = total.cost, # Parameters for the inequality constraint
            ineqUB = B, # Upper bound of inequality / constraint
            ineqLB = 0, # Lower bound of inequality /constraint
            LB = rep(0,N), # Lower bound for decision variables, must be a vector / parameters 
            UB = rep(1,N)) # Upper bound for decision variables, must be a vector / parameters
OO

# Retrieve optimal lot purchase fractions
F_star <- OO$pars
# Estimate total cost
TC_star <- F_star%*%c
TC_star


