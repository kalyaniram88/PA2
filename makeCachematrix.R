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
    # check if the inverse is already cached
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # not cached, so we get the matrix into data
    data <- x$get()
    # and compute the inverse
    inv <- solve(data, ...)
    # then cache the inverse
    x$setinv(inv)
    # and return it as well
    inv
}
