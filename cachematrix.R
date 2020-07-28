## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Assuming Matrix is invertable
  inv <- NULL
  set <- function(z){
    x <<- z
    inv <<- NULL
    }
  get <- function() x
  setInverse <- function(inverse){inv<<- inverse}
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)){
          message("Matrix inverse already solved, getting cache data")
          return(inv)
      }
      matx<- x$get()
      inv<- solve(matx, ...)
      x$setInverse(inv)
      inv
}
