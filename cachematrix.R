## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                x_inv <- NULL
                set <- function(y) {
                        x <<- y
                        x_inv <<- NULL
                }
                get <- function() x
                set_inv <- function(solve) x_inv <<- solve
                get_inv <- function() x_inv
                list(set = set, get = get,
                     set_inv = set_inv,
                     get_inv = get_inv)
                
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$get_inv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$set_inv(x_inv)
        x_inv
}
