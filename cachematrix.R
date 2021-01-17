## There are two functions here, the first function creates a list to set the 
## value of the matrix, get the matrix
## it then sets the value of the inverse, and gets the value of the inverse
## the second function solves for the inverse of the matrix, defined in the
## first

makeCacheMatrix <- function(x = matrix()) {
  INV<-NULL     # This clears any previous values of the Inverse   
  set <- function(y) {
    x <<- y 
    INV <<- NULL 
  }
  get <- function() x 
  setInverse <- function(Inverse) INV <<- Inverse  
  getInverse <- function () INV 
  list (set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
  }


## This function "Solves" for the inverse of the matrix
## If the function has already been solved, it pulls the cached Matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  INV <- x$getInverse()       #Attempts to pull the established Inverse 
  if(!is.null(INV)){
      message("getting cached data")
      return(INV)
  }
  data <- x$get()     ##This pulls the defined matrix 
  INV <- solve(data, ...)   ##Solves for the Inverse 
  x$setInverse(INV)    ## Sets the value of the Inverse 
  INV                 #Prints the Inverse
  
}
