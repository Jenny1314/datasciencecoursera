## Caching the Inverse of a Matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getmatrix <- function() {
        x
    }
    setinverse <- function(i){
        inverse <<- i
    } 
    getinverse<- function(){
        inverse
    } 
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$getmatrix()
    detofmatrix<-det(matrix)
    if(detofmatrix>0){
        i<-solve(matrix, ...)
        x$setinverse(i)
        return(i)
    }
    else{
        i<-"Singular matrix is not invertible"
        x$setinverse(i)
        return(i)
    } 
    i
}
