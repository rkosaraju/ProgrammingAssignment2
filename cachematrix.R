## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list containing a funtion to 
# 1) set the value of the matrix supplied
# 2) get the value of the matrix supplied
# 3) set the inverse of the matrix 
# 4) get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  


## Write a short comment describing this function
# cacheSolve takes as an input a square matrix
# and checks to see if a value for this already exists in
# the cache. It makes use of the function created above in the 
# makeCacheMatrix to retrieve the value. If no value was found
# in the cache, it will calculate using the solve function and
# store it in the cache and will also return as the return value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting inverse of matrix")
        return(m)
    }
    data <- x$get()
    m <- data %*% solve(data, ...)
    x$setinverse(m)
    m
}
