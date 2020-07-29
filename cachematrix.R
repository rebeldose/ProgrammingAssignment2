##  Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##  1. set the value of the matrix
##  2.get the value of the matrix
##  3.set the value of the inverse
##  4.get the value of the inverse

##The second function calculates the inverse of the special "matrix" created with the first function.
##it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.


## The first function, makeCacheMatrix creates a special "matrix",which is really a list.

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {i <<- inverse}
        getinverse <- function() {i}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function, cacheSolve calculates the inverse of the special "matrix" created with the above function.

cacheSolve <- function(x, ...){
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}



