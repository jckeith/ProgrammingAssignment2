## These 2 functions work together to create a matrix and calculate its inverse, but also to cache
##its inverse so that it doesnt have to be calculated again

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<-inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the matrix created in makeCacheMatrix. However, it first checks
## to see if the inverse has already been calculated and if it has it skips the calculation 
## and calls the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}
