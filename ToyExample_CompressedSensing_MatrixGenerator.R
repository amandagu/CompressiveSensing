#Set parameters
dimension <- 5
set.seed(10)

#Generate a random matrix
Randomifier <- matrix(rnorm(dimension*dimension, 0, 1), ncol = dimension)

#Make this matrix sparse
Sparsifier <- runif(dimension*dimension)
index <-1

for(r in 1:dimension){
  for(c in 1:dimension){
    if(Sparsifier[index]<0.5){
      Randomifier[r,c] <- 0
    }
    index <- index+1
  }
}

#Create a random singular matrix
Singularifier <- diag(dimension)

x <- sample(1:dimension, 3, replace = FALSE)
  
Singularifier[x[3],] <- Singularifier[x[1],] + Singularifier[x[2],]

#Multiply by a singular matrix by random, sparse matrix to get a singular, random, sparse matrix 
Func <- Singularifier %*% Randomifier

#Since Func is singular, there exists a non-trivial solution to the system b=Func*x, where b is the 0 vector