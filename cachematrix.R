# This function allows the user to cache a potentially time-consuming calculation
# such as taking the inverse of a matrix. Matrix inversion is usually a costly 
# computation and there may be some benefit to caching the inverse of a matrix
# rather than compute it repeatedly. The following two functions are used to 
# cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
#      - set the value of the matrix
#      - get the value of the matrix
#      - set the value of inverse of the matrix
#      - get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve will retrieve the inverse cache.
# Otherwise it will compute the inverse then set the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
