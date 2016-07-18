##Given an invertible matrix the following function will calculate the inverse matrix or retrieve the inverse 
## matrix from the cache.

## Function "makeCacheMatrix" creates a special matrix object that can cache its inverse. This object has four "functions: set, get, 
setinverse and getinverse

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

## Function “cacheSolve” computes the inverse of the special “matrix” returned by makeCacheMatrix above.
If the inverse has already been calculated, the cacheSolve should retrieve the inverse from the cache. If the inverse has not
been calculated data gets the matrix stored with makeCacheMatrix; m calculates the inverse and x$setinverse(m) stores it in the 
object "m" in makeCachematrix

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
}
Testing the functions
> A <- diag(7, 4)
> A
     [,1] [,2] [,3] [,4]
[1,]    7    0    0    0
[2,]    0    7    0    0
[3,]    0    0    7    0
[4,]    0    0    0    7
> CachedMatrix <- makeCacheMatrix(A)
> cacheSolve(CachedMatrix)
          [,1]      [,2]      [,3]      [,4]
[1,] 0.1428571 0.0000000 0.0000000 0.0000000
[2,] 0.0000000 0.1428571 0.0000000 0.0000000
[3,] 0.0000000 0.0000000 0.1428571 0.0000000
[4,] 0.0000000 0.0000000 0.0000000 0.1428571
> B = matrix(c(2, 4, 3, 1, 5, 7),
+ nrow=2,
+ ncol=2)
> B
     [,1] [,2]
[1,]    2    3
[2,]    4    1
> CachedMatrix2 <- makeCacheMatrix(B)
> cacheSolve(CachedMatrix2)
     [,1] [,2]
[1,] -0.1  0.3
[2,]  0.4 -0.2

}
