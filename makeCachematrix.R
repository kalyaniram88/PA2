makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   # initialize the stored inverse value to NULL
    set <- function(y) {     # to set the value of the matrix
        x <<- y
        inv <<- NULL # since the matrix changed
    }
   
    get <- function() x
    # to set the inverse
    setinv <- function(inv_) inv <<- inv_
    # to get the inverse
    getinv <- function() inv

    # return a list of all the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
