#Using L1 minimization to find min||C||_1 s.t. ||A*C-V||_2 < epsilon
library(ADMM)
C <- matrix(0, nrow = noMonomial, ncol = dimension)

AA <- A[[1]]
VV <- V[[1]][,-1]
for (i in 2:K){
  AA <- rbind(AA,A[[i]])
  VV <- rbind(VV,V[[i]][,-1])
}

time <- 0

for(i in 1:dimension){
  
  bb <- VV[,i]
  
  output = admm.bp(A, b)
  C[,i] <- result[[1]]
  timeCVXR <- time+result[[8]]
  print(C[,i])
}  
V_hat <- AA%*%C

distanceCVXR<-norm(VV-V_hat, type =  "2")
