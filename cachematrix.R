## The following functions solve for the inverse of a square matrix
## and allow for caching of said inverse, for easy retrieval.

## the makeCacheMatrix function takes a matrix as input, and 
## provides the get... and setinverse functions to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function() m <<- solve(x)
     getinverse <- function() m
     list(set = set, get = get, 
          setinverse = setinverse, getinverse = getinverse)
}

## the cacheSolve function either shows the inverse of the matrix 
## it takes as an argument, or, if the inverse has been cached, 
## retrieves the cached matrix

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m
}
