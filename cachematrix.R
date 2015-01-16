## The two functions before help a user to create & update a square matrix 
## and then calculate the inverse matrix or pull it from cache if the matrix 
## was not updated and the inverse matrix was already calculated.

## The makeCacheMatrix function is used to help de user to create, visualize 
## and update a matrix used (x) and the corresponding inverse one (x_inv). The inverse matrix 
## is calculated with the help of solve() available R function and it is saved in cache (different environment).

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


## The cacheSolve function is used to retrive the cached inverse matrix or 
## calculate the inverse matrix for the first time with the solve function 
## and store the result in cache.

cacheSolve <- function(x, ...) {
        
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