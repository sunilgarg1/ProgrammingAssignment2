## Matrix inversion is usually a costly computation and hence it may be beneficial to
## cache the inverse of a matrix rather than computing it repeatedly, to speed up the inversion.
## makeCacheMatrix caches the inverse of a matrix and cacheSolve returns the inverse from the cache
## if it is available in the cache, else it will compute the inverse and then cache it and return it.
## Both these functions assume a square invertible matrix. The inverse of a square matrix can be calculated
## by using the function solve in Matrix package. This package needs to be installed and loaded in order for
## these functions to work. It is also useful to install and load the package matrixcalc to test if the matrix 
## is indeed a square matrix using is.square.matrix() function.


library(Matrix)
library(matrixcalc)
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matrixInverse <<- inverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Else it will compute the inverse, cache it and then return the newly computed inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    
    
    matrixInverse <- solve(x$get())
    x$setInverse(matrixInverse)
    matrixInverse
}
