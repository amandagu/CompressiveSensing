#Now define this matrix as an ODE function
library(deSolve)

dxi_dt <- function(t, state, parameters){
  state <- as.vector(state)
  v <- Func %*% state
  v <- list(v)
  return(v)
}

#Define bursts, in this case 3 bursts of length 5, and delta
K <- 3
M <- 5
delta <- 0.1

times <- seq(from = 0, by = delta, length.out = M)

#Set random initial values
Init <- matrix(0, nrow = dimension, ncol = K)
for(r in 1:nrow(Init)) {
  for(c in 1:ncol(Init)) {
    Init[r,c] <- rexp(1,1.5)
  }
}

#Determine state at given steps
out <- matrix(0, nrow = M, ncol = dimension+1)
X <- rep(list(out),K)
for(k in 1:K){
  X[[k]] <- ode(y = Init[,k], times = times, func = dxi_dt, parms = Func)
}

#Determine velocity at given steps
V <- X
for(k in 1:K){
  for(m in 2:M+1){
    V[[k]][,m] <- Func %*% X[[k]][,m]
  }
}
