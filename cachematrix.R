## makeCacheMatrix is a function that takes a square matrix as an argument and returns an object with a group of functions actionable upon it.
## this group of functions include set, get, setInverse, and getInverse
## by returning a list, the object's set of functions can be accessed by using the $ symbol

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      setInverse <- function(Inverse) m <<- Inverse
      getInverse <- function() m
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setInverse(m)
      m
}