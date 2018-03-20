There are two functions required for this assignment. "makeCacheMatrix" and "cacheSolve".
These functions are made to facilitate the costly computation of matrix inversion by caching it. 

There are two functions required for this assignment. "makeCacheMatrix" and "cacheSolve".

makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { m <- NULL set <- function(y) { x <<- y m <<- NULL } get <- function() x setinv <- function(solve) m <<- solve getinv <- function() m list(set = set, get = get, setinv = setinv, getinv = getinv) } m <- matrix(c(1,2,3,4),2,2)

m1<-makeCacheMatrix(m)

cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve<-function(x,...){ m<-x$getinv() if(!is.null(m)){ message("getting cached data") return(m) } data<- x$get() m<-solve(data,...) x$setinv(m) m }

Test the functions: m <- matrix(c(1,2,3,4),2,2) m1<-makeCacheMatrix(m) cacheSolve(m1)

 [,1] [,2]
[1,] -2 1.5 [2,] 1 -0.5

© 2018 GitHub, Inc.
