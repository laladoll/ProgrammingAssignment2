#Caching Inverse Matrix

### M.Yoshino
### 21 March 2018


##Assignment: Caching the Inverse of a Matrix

###This assignment requires to write two functions in below which will cache the inverse of a matrix. The aim is to save the costly process of a matrix inversion by caching it, instead of computing it over and over. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
###1.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
### **The underlying assumption of this assignment is that the matrix is always invertible.**
###The first function,makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
###1.	set the value of the matrix
###2.	get the value of the matrix
###3.	set the value of the inverse
###4.	get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set,
get = get,
setinverse = setinverse,
getinverse = getinverse)
}

###The second function,cacheSolve,computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 i <- x$getinverse()
 if (!is.null(i)) {
 message("getting cached data")
 return(i)
 }
 data <- x$get()
 i <- solve(data, ...)
 x$setinverse(i)
 i
}

##Testing
### The above functions will be tested by calling the functions with a matrix. 

M <- matrix(c(1,2,3,4),2,2)
M1 <- makeCacheMatrix(M)
cacheSolve(M1) #inverse returned after computation
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
cacheSolve(M1) #inverse returned from cache
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
