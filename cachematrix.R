## These two function will help to calculate the inverse of the matrix and next time if 
## functions gets the same matrix to calculate the inverse it will return the previously calculated inverse value.

## This function will hold matrix for calculating the inverse and further helps to get inverse and modify the matrix to calculate new inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates inverse of the matrix and if this function is already been called for the 
## same element then function returns previously calculated inverse. That is, it stores inverse value 
## in global environment. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
