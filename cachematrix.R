## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The makeCacheMatrix function allows for:
# - Getting the value of a matrix
# - Setting the inverse value of a matrix
# - Getting the inverse value of a matrix
# Used the Assignment example as a guide. Removed the "set" function that was not being used
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# Function cacheSolve checks for an existing "cached" inverse matrix.  
# it returns such matrix if it is present. Otherwise it performs the inversion of
# the given matrix and return it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
