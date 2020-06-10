#Using L1 minimization to find min||C||_1 s.t. ||A*C-V||_2 < epsilon
library(CVXR)
C <- matrix(0, nrow = noMonomial, ncol = dimension)

objective <- Minimize(norm(C_iMatrix, type = "1"))
equalities <- matrix(quote(A[[k]][m,] %*% C[,i] == V[[k]][m,i]), nrow=1, ncol = K*M*dimension)
constraints <- vector(mode="list", length = K*M*dimension)
index <- 1

for(i in 1:dimension){
  for(k in 1:K){
    for(m in 1:M){
      equalities[[index]] <- quote(A[[k]][m,] %*% C[,i] == V[[k]][m,i])
      index <- index + 1
    }
  }
}

prob2.1 <- Problem(objective, constraints)

# Problem solution
solution2.1 <- solve(prob2.1)
solution2.1$status