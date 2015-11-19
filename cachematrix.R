
## Below are two functions that are used to create an object that 
## stores a matrix object and cache's its inverse.


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ## Function to change the matrix stored in the function
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## Function to return the matrix stored in the funtion
      get <- function() x
      ## Function to store the inverse of the matrix into m
      setinverse <- function(solve) m <<- solve
      ## Function to return the inverse of the matrix stored in the function
      getinverse <- function() m 
      ## store the 4 functions
      list(set = set, get = get,
           setinverse = setinverse, 
           getinverse = getinverse)
}



## This function calculates the inverse of the matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()     
                              ## Verify the value m, exists and is not NULL. 
                              ## If it exists in memory, return a message, 
                              ## the value m and exit the function.
      if(!is.null(m)) {  
            message("getting cached data")
            return(m)
      }
                              ## otherwise
      data <- x$get()         ## retrieve the matrix.
      m <- solve(data,...)    ## calc the inverse of the matric.
      x$setinverse(m)         ## cache the value in the object created with
                              ## makeCacheMatrix.
      m                       ## return the inverted matrix.
}
