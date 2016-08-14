## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## The below function computes inverse of a matrix and cache them
## Load library MASS as function from the packages have been used

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(ginv) i <<- ginv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 4
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- ginv(data, ...)
        x$setInv(i)
        i
}

