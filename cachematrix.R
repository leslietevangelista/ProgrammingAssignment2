## Two functions that cache the inverse of a matrix
## 

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(a){
    z <<- a
    inv <<- NULL
  }
  get <- function() a
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function below calculates the inverse of makeCacheMatrix above

cacheSolve <- function(a, ...) {
        ## intends to return the inverse of the function 'a'
     inv <- a$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data)
  a$setInverse(inv)
  inv         
}

