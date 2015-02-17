## In order to save computation time this pair of functions will cache the inverse of
## a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
## The function stores four methods for getting access to the matrix and its inverse.
## The four methods are set, get, setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
      
      # Erase any cached value that might exist in 'inverse'
      inverse <- NULL
      
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(new.inverse) inverse <<- new.inverse
      getinverse <- function() inverse
      list(setinverse = setinverse,
           getinverse = getinverse,
           set = set,
           get = get)    
}


## This function takes a matrix object as its parameter and uses the object's
## methods/functions to get access to the matrix itself and its stored inverse.
## If it exists, the function will return the cached inverse; otherwise it will
## calculate a new inverse of the matrix and return that new inverse.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      
      ## If 'inverse' object is not null, show the cached date
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      ## If 'inverse' object is null, calculate the inverse and show that
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}