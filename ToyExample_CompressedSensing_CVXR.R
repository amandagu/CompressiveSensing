  #Using L1 minimization to find min||C||_1 s.t. ||A*C-V||_2 < epsilon
  library(CVXR)
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
    
    C_iHat <- Variable(rows=noMonomial, cols=1)
    objective <- Minimize(norm(C_iHat, type = "1"))
    print(bb)
    ones<-bb
    for(j in 1:length(bb)){
      bb[j]=1
    }
    problem <- Problem(objective, constraints = list(AA%*%C_iHat==bb))
    
    result <- solve(problem)
    C[,i] <- result[[1]]
    timeCVXR <- time+result[[8]]
    print(C[,i])
 }  
  V_hat <- AA%*%C
  
  distanceCVXR<-norm(VV-V_hat, type =  "2")
  