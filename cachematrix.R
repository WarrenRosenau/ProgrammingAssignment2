## Put comments here that give an overall description of what your
## functions do

## Given a matrix, this function returns an object(list) containing the 
## original matrix and functions to get/set the matrix and get/set the inverse 
## of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Given a cacheMatrix object(list) created by makeCacheMatrix, this function will 
## return the cached inverse of the contained matrix if there is a cached copy or
## it will compute the inverse and store it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
