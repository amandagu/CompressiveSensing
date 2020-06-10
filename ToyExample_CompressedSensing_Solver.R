#Given have V (velocities), and Init (initial values)
#Goal is to solve for func, assuming everyone is quadratic (which we have been)

temp <- dimension+1
noMonomial <- (((temp)^2-temp)/2)+temp

#Define empty list to be filled with matricies A^(k)
A <- vector(mode = "list", length = K)

#Fill A^(k) with the given monomial bases
for(k in 1:K){
  A[[k]] <- matrix(0,nrow=M,ncol=noMonomial)
  
  aCol <- 1
  
  for(row in 1:M){
    for(c1 in 1:ncol(X[[k]])){
      if(c1==1){
        var1 <- 1
      }
      else{
        var1 <- X[[k]][row,c1]
      }
      
      for(c2 in c1:ncol(X[[k]])){
        if(c2==1){
          var2 <- 1
        }
        else{
          var2 <- X[[k]][row,c2]
        }
        
        A[[k]][row,aCol] <- var1*var2
        aCol <- aCol+1
      }
    }
    aCol <- 1
  }
}